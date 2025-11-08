// Initially based on https://github.com/SparkyPotato/radiance/tree/main/ext/slang-rs

use std::{
	ffi::{CStr, CString, c_char, c_void},
	mem, ptr, slice,
	str::FromStr,
	sync::atomic::{AtomicU32, Ordering},
};

use crate::{self as slang, Interface as _};
use shader_slang_sys::{self as sys};

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SlangPathType {
	Directory,
	File,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PathKind {
	/// Given a path, returns a simplified version of that path.
	/// This typically means removing '..' and/or '.' from the path.
	/// A simplified path must point to the same object as the original.
	Simplified,

	/// Given a path, returns a 'canonical path' to the item.
	/// This may be the operating system 'canonical path' that is the unique path to the item.
	///
	/// If the item exists the returned canonical path should always be usable to access the
	/// item.
	///
	/// If the item the path specifies doesn't exist, the canonical path may not be returnable
	/// or be a path simplification.
	/// Not all file systems support canonical paths.
	Canonical,

	/// Given a path returns a path such that it is suitable to be displayed to the user.
	///
	/// For example if the file system is a zip file - it might include the path to the zip
	/// container as well as the path to the specific file.
	///
	/// NOTE! The display path won't necessarily work on the file system to access the item
	Display,

	/// Get the path to the item on the *operating system* file system, if available.
	OperatingSystem,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OSPathKind {
	/// Paths do not map to the file system
	None,
	/// Paths map directly to the file system
	Direct,
	/// Only paths gained via PathKind::OperatingSystem map to the operating
	/// system file system
	OperatingSystem,
}

type FileSystemContentsCallBack =
	unsafe extern "C" fn(path_type: SlangPathType, name: *const c_char, user_data: *const c_void);

pub trait FileSystem: Send {
	fn load_file(&self, path: &str) -> std::io::Result<Box<[u8]>>;
}

pub trait FileSystemExt: Send {
	fn load_file(&self, path: &str) -> std::io::Result<Box<[u8]>>;
	fn get_file_unique_identity(&self, path: &str) -> std::io::Result<String>;
	fn calc_combined_path(
		&self,
		from_path_type: SlangPathType,
		from_path: &str,
		path: &str,
	) -> std::io::Result<String>;
	fn get_path_type(&self, path: &str) -> std::io::Result<SlangPathType>;
	fn get_path(&self, kind: PathKind, path: &str) -> std::io::Result<String>;
	fn clear_cache(&self);
	fn enumerate_path_contents(
		&self,
		path: &str,
		callback: &dyn Fn(SlangPathType, &str),
	) -> std::io::Result<()>;
	fn get_os_path_kind(&self) -> OSPathKind;
}

const FILE_SYSTEM_IID: slang::UUID = slang::uuid(
	0x003A09FC,
	0x3A4D,
	0x4BA0,
	[0xAD, 0x60, 0x1F, 0xD8, 0x63, 0xA9, 0x15, 0xAB],
);

const FILE_SYSTEM_EXT_IID: slang::UUID = slang::uuid(
	0x5fb632d2,
	0x979d,
	0x4481,
	[0x9f, 0xee, 0x66, 0x3c, 0x3f, 0x14, 0x49, 0xe1],
);

#[repr(transparent)]
#[derive(Clone)]
struct Castable(slang::IUnknown);

unsafe impl slang::Interface for Castable {
	type Vtable = slang::sys::ICastableVtable;
	const IID: slang::UUID = slang::uuid(
		0x8f241361,
		0xf5bd,
		0x4ca0,
		[0xa3, 0xac, 0x02, 0xf7, 0xfa, 0x24, 0x02, 0xb8],
	);
}

#[repr(C)]
struct ISlangFileSystemVtable {
	unknown: sys::ISlangUnknown__bindgen_vtable,
	cast_as:
		unsafe extern "C" fn(this: *mut sys::ISlangCastable, guid: &slang::UUID) -> *mut c_void,
	load_file: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		out_blob: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult,
}

#[repr(C)]
pub(crate) struct FileSystemImpl {
	vtable: *const ISlangFileSystemVtable,
	ref_count: AtomicU32,
	inner: Box<dyn FileSystem>,
}

impl FileSystemImpl {
	const VTABLE: ISlangFileSystemVtable = ISlangFileSystemVtable {
		unknown: sys::ISlangUnknown__bindgen_vtable {
			ISlangUnknown_queryInterface: Self::query_interface,
			ISlangUnknown_addRef: Self::add_ref,
			ISlangUnknown_release: Self::release,
		},
		cast_as: Self::cast_as,
		load_file: Self::load_file,
	};

	#[inline]
	pub(crate) fn new(inner: Box<dyn FileSystem>) -> Self {
		Self {
			vtable: &Self::VTABLE,
			ref_count: AtomicU32::new(1),
			inner,
		}
	}

	unsafe extern "C" fn query_interface(
		this: *mut sys::ISlangUnknown,
		uuid: *const slang::UUID,
		out_object: *mut *mut c_void,
	) -> sys::SlangResult {
		unsafe {
			query_interface_impl(
				this,
				uuid,
				out_object,
				&[FILE_SYSTEM_IID, Castable::IID, slang::IUnknown::IID],
			)
		}
	}

	unsafe extern "C" fn add_ref(this: *mut sys::ISlangUnknown) -> u32 {
		unsafe {
			let prev = (*(this as *mut FileSystemImpl))
				.ref_count
				.fetch_add(1, Ordering::SeqCst);
			prev + 1
		}
	}
	unsafe extern "C" fn release(this: *mut sys::ISlangUnknown) -> u32 {
		unsafe {
			let prev = (*(this as *mut FileSystemImpl))
				.ref_count
				.fetch_sub(1, Ordering::SeqCst);
			if prev == 1 {
				let _ = Box::from_raw(this as *mut FileSystemImpl);
				0
			} else {
				prev - 1
			}
		}
	}

	unsafe extern "C" fn cast_as(
		this: *mut sys::ISlangCastable,
		guid: &slang::UUID,
	) -> *mut c_void {
		let mut object = ptr::null_mut();
		if unsafe { Self::query_interface(this.cast(), guid as *const _, &mut object) } == S_OK {
			object
		} else {
			ptr::null_mut()
		}
	}

	unsafe extern "C" fn load_file(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		out_blob: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult {
		unsafe {
			if out_blob.is_null() || path.is_null() {
				return E_INVALIDARG;
			}

			let wrapper = &(*this.cast::<FileSystemImpl>()).inner;

			let path = CStr::from_ptr(path).to_string_lossy();

			match wrapper.load_file(&path) {
				Ok(blob) => {
					// *Copy* the data into a Slang-owned blob.
					let blob = slang::sys::slang_createBlob(blob.as_ptr().cast(), blob.len());
					out_blob.write(blob);
					S_OK
				}
				Err(error) => {
					*out_blob = ptr::null_mut();
					match error.kind() {
						std::io::ErrorKind::NotFound => E_NOT_FOUND,
						_ => E_CANNOT_OPEN,
					}
				}
			}
		}
	}
}

#[repr(C)]
struct ISlangFileSystemExtVtable {
	base: ISlangFileSystemVtable,
	get_file_unique_identity: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		out_unique_identity: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult,
	calc_combined_path: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		from_path_type: SlangPathType,
		from_path: *const c_char,
		path: *const c_char,
		path_out: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult,
	get_path_type: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		path_type_out: *mut SlangPathType,
	) -> sys::SlangResult,
	get_path: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		kind: PathKind,
		path: *const c_char,
		out_path: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult,
	clear_cache: unsafe extern "C" fn(this: *mut sys::ISlangFileSystem),
	enumerate_path_contents: unsafe extern "C" fn(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		callback: FileSystemContentsCallBack,
		user_data: *const c_void,
	) -> sys::SlangResult,
	get_os_path_kind: unsafe extern "C" fn(this: *mut sys::ISlangFileSystem) -> OSPathKind,
}

#[repr(C)]
pub(crate) struct FileSystemExtImpl {
	vtable: *const ISlangFileSystemExtVtable,
	ref_count: AtomicU32,
	inner: Box<dyn FileSystemExt>,
}

impl FileSystemExtImpl {
	const VTABLE: ISlangFileSystemExtVtable = ISlangFileSystemExtVtable {
		base: ISlangFileSystemVtable {
			unknown: sys::ISlangUnknown__bindgen_vtable {
				ISlangUnknown_queryInterface: Self::query_interface,
				ISlangUnknown_addRef: Self::add_ref,
				ISlangUnknown_release: Self::release,
			},
			cast_as: Self::cast_as,
			load_file: Self::load_file,
		},
		get_file_unique_identity: Self::get_file_unique_identity,
		calc_combined_path: Self::calc_combined_path,
		get_path_type: Self::get_path_type,
		get_path: Self::get_path,
		clear_cache: Self::clear_cache,
		enumerate_path_contents: Self::enumerate_path_contents,
		get_os_path_kind: Self::get_os_path_kind,
	};

	#[inline]
	pub(crate) fn new(inner: Box<dyn FileSystemExt>) -> Self {
		Self {
			vtable: &Self::VTABLE,
			ref_count: AtomicU32::new(1),
			inner,
		}
	}

	unsafe extern "C" fn query_interface(
		this: *mut sys::ISlangUnknown,
		uuid: *const slang::UUID,
		out_object: *mut *mut c_void,
	) -> sys::SlangResult {
		unsafe {
			query_interface_impl(
				this,
				uuid,
				out_object,
				&[
					FILE_SYSTEM_EXT_IID,
					FILE_SYSTEM_IID,
					Castable::IID,
					slang::IUnknown::IID,
				],
			)
		}
	}

	unsafe extern "C" fn add_ref(this: *mut sys::ISlangUnknown) -> u32 {
		unsafe {
			let prev = (*(this as *mut FileSystemExtImpl))
				.ref_count
				.fetch_add(1, Ordering::SeqCst);
			prev + 1
		}
	}
	unsafe extern "C" fn release(this: *mut sys::ISlangUnknown) -> u32 {
		unsafe {
			let prev = (*(this as *mut FileSystemExtImpl))
				.ref_count
				.fetch_sub(1, Ordering::SeqCst);
			if prev == 1 {
				let _ = Box::from_raw(this as *mut FileSystemExtImpl);
				0
			} else {
				prev - 1
			}
		}
	}

	unsafe extern "C" fn cast_as(
		this: *mut sys::ISlangCastable,
		guid: &slang::UUID,
	) -> *mut c_void {
		let mut object = ptr::null_mut();
		if unsafe { Self::query_interface(this.cast(), guid as *const _, &mut object) } == S_OK {
			object
		} else {
			ptr::null_mut()
		}
	}

	unsafe fn wrapper<'a>(
		this: *mut shader_slang_sys::ISlangFileSystem,
	) -> &'a (dyn FileSystemExt + 'static) {
		unsafe { &*(*this.cast::<FileSystemExtImpl>()).inner }
	}

	unsafe extern "C" fn load_file(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		out_blob: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult {
		unsafe {
			if out_blob.is_null() || path.is_null() {
				return E_INVALIDARG;
			}

			Self::io_result_to_slang(
				Self::wrapper(this).load_file(&CStr::from_ptr(path).to_string_lossy()),
				Self::box_to_blob,
				Some(out_blob),
			)
		}
	}

	unsafe extern "C" fn get_file_unique_identity(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		out_unique_identity: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult {
		unsafe {
			Self::io_result_to_slang(
				Self::wrapper(this)
					.get_file_unique_identity(&CStr::from_ptr(path).to_string_lossy()),
				Self::string_to_blob,
				Some(out_unique_identity),
			)
		}
	}

	unsafe extern "C" fn calc_combined_path(
		this: *mut sys::ISlangFileSystem,
		from_path_type: SlangPathType,
		from_path: *const c_char,
		path: *const c_char,
		path_out: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult {
		unsafe {
			Self::io_result_to_slang(
				Self::wrapper(this).calc_combined_path(
					from_path_type,
					&CStr::from_ptr(from_path).to_string_lossy(),
					&CStr::from_ptr(path).to_string_lossy(),
				),
				Self::string_to_blob,
				Some(path_out),
			)
		}
	}

	unsafe extern "C" fn get_path_type(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		path_type_out: *mut SlangPathType,
	) -> sys::SlangResult {
		unsafe {
			Self::io_result_to_slang(
				Self::wrapper(this).get_path_type(&CStr::from_ptr(path).to_string_lossy()),
				std::convert::identity,
				Some(path_type_out),
			)
		}
	}

	unsafe extern "C" fn get_path(
		this: *mut sys::ISlangFileSystem,
		kind: PathKind,
		path: *const c_char,
		out_path: *mut *mut slang::sys::ISlangBlob,
	) -> sys::SlangResult {
		unsafe {
			Self::io_result_to_slang(
				Self::wrapper(this).get_path(kind, &CStr::from_ptr(path).to_string_lossy()),
				Self::string_to_blob,
				Some(out_path),
			)
		}
	}

	unsafe extern "C" fn clear_cache(this: *mut sys::ISlangFileSystem) {
		unsafe {
			Self::wrapper(this).clear_cache();
		}
	}
	unsafe extern "C" fn enumerate_path_contents(
		this: *mut sys::ISlangFileSystem,
		path: *const c_char,
		callback: FileSystemContentsCallBack,
		user_data: *const c_void,
	) -> sys::SlangResult {
		unsafe {
			Self::io_result_to_slang::<(), (), _>(
				Self::wrapper(this).enumerate_path_contents(
					&CStr::from_ptr(path).to_string_lossy(),
					&|path_type, name| {
						callback(
							path_type,
							CString::from_str(name).unwrap().as_ptr(),
							user_data,
						);
					},
				),
				std::convert::identity,
				None,
			)
		}
	}

	unsafe extern "C" fn get_os_path_kind(this: *mut sys::ISlangFileSystem) -> OSPathKind {
		unsafe { Self::wrapper(this).get_os_path_kind() }
	}

	fn string_to_blob(s: String) -> *mut shader_slang_sys::ISlangBlob {
		unsafe {
			let blob = s.as_bytes();
			// *Copy* the data into a Slang-owned blob.
			slang::sys::slang_createBlob(blob.as_ptr().cast(), blob.len())
		}
	}

	#[allow(clippy::boxed_local)]
	fn box_to_blob(b: Box<[u8]>) -> *mut shader_slang_sys::ISlangBlob {
		unsafe {
			// *Copy* the data into a Slang-owned blob.
			slang::sys::slang_createBlob(b.as_ptr().cast(), b.len())
		}
	}

	unsafe fn io_result_to_slang<R, SR, F: FnOnce(R) -> SR>(
		res: Result<R, std::io::Error>,
		map_result: F,
		out_blob: Option<*mut SR>,
	) -> i32 {
		unsafe {
			match res {
				Ok(blob) => {
					let blob = map_result(blob);
					if let Some(out_blob) = out_blob {
						out_blob.write(blob);
					}
					S_OK
				}
				Err(error) => match error.kind() {
					std::io::ErrorKind::NotFound => E_NOT_FOUND,
					_ => E_CANNOT_OPEN,
				},
			}
		}
	}
}

const SLANG_FACILITY_BASE: i32 = 0x200;
const SLANG_FACILITY_CORE: i32 = SLANG_FACILITY_BASE;

const fn slang_make_error(fac: i32, code: i32) -> sys::SlangResult {
	let fac_u = fac as u32;
	let code_u = code as u32;
	let v = (fac_u << 16) | code_u | 0x8000_0000;
	v as i32
}

const fn slang_make_core_error(code: i32) -> sys::SlangResult {
	slang_make_error(SLANG_FACILITY_CORE, code)
}

const S_OK: sys::SlangResult = 0;
const E_INVALIDARG: sys::SlangResult = -2147024809;
const E_NOINTERFACE: sys::SlangResult = -2147467262;
const E_CANNOT_OPEN: sys::SlangResult = slang_make_core_error(4);
const E_NOT_FOUND: sys::SlangResult = slang_make_core_error(5);

unsafe fn query_interface_impl(
	this: *mut sys::ISlangUnknown,
	uuid: *const slang::UUID,
	out_object: *mut *mut c_void,
	supported_interfaces: &[slang::UUID],
) -> sys::SlangResult {
	unsafe {
		if out_object.is_null() {
			return E_INVALIDARG;
		} else {
			*out_object = std::ptr::null_mut();
		}

		if uuid.is_null() {
			return E_INVALIDARG;
		}

		let lhs = slice::from_raw_parts(uuid.cast::<u8>(), mem::size_of::<slang::UUID>());

		let eq = |id: &slang::UUID| {
			let rhs = slice::from_raw_parts(
				(id as *const slang::UUID).cast::<u8>(),
				mem::size_of::<slang::UUID>(),
			);
			lhs == rhs
		};

		for iface in supported_interfaces {
			if eq(iface) {
				((*(*this).vtable_).ISlangUnknown_addRef)(this);
				*out_object = this.cast();
				return S_OK;
			}
		}

		E_NOINTERFACE
	}
}
