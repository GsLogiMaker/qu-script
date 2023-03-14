
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::size_of;
use std::slice::SliceIndex;

use crate::ExternalFunction;
use crate::QuMsg;
use crate::QuRegisterStruct;
use crate::QuVoid;
use crate::compiler::ConstantId;
use crate::compiler::ExternalFunctionId;
use crate::compiler::FunctionGroupId;
use crate::compiler::FunctionId;
use crate::compiler::FunctionIdentity;
use crate::compiler::SomeFunctionGroup;
use crate::compiler::SomeFunctionId;


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
/// An ID for an external function.
pub struct QuExtFnId(pub usize);
impl QuExtFnId {

	/// Constructs a new [`QuExtFnId`].
	pub fn new(index:usize) -> Self {
		Self(index)
	}

} impl From<usize> for QuExtFnId {

	fn from(v:usize) -> Self {
		QuExtFnId(v)
	}

}


impl From<QuExtFnId> for u8 {
	fn from(v:QuExtFnId) -> Self {
		v.0 as u8
	}
}


#[derive(Clone, Debug, Default)]
pub struct QuRegistered {
	pub fns:Vec<ExternalFunction>,
	pub structs:Vec<QuStruct>,
	pub structs_map:HashMap<String, ClassId>,
} impl QuRegistered {

	/// Constructs a new [`QuImports`].
	pub fn new() -> Self {
		Self {
			fns: Vec::default(),
			structs: Vec::default(),
			structs_map: HashMap::default(),
		}
	}
	

	pub fn get_fn_data_by_id(&self, fn_id:FunctionId
	) -> Result<&ExternalFunction, QuMsg>{
		match self.fns.get(fn_id) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external function by id `{:?}` is registered.",
				fn_id
			).into()),
		}
	}


	pub fn get_struct<T:QuRegisterStruct>(
		&self,
	) -> Result<&QuStruct, QuMsg>{
		self.get_struct_by_str(<T as QuRegisterStruct>::name())
	}


	pub fn get_struct_by_str(&self, name:&str
	) -> Result<&QuStruct, QuMsg>{
		Ok(self.get_struct_by_id(self.get_struct_id_by_str(name)?)?)
	}


	pub fn get_struct_by_id(&self, id:ClassId
	) -> Result<&QuStruct, QuMsg>{
		match self.structs.get(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_by_id_mut(&mut self, id:ClassId
	) -> Result<&mut QuStruct, QuMsg>{
		match self.structs.get_mut(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_id<T:QuRegisterStruct>(
		&self
	) -> Result<ClassId, QuMsg> {
		self.get_struct_id_by_str(<T as QuRegisterStruct>::name())
	}


	pub fn get_struct_id_by_str(&self, name:&str
	) -> Result<ClassId, QuMsg>{
		let Some(d) = self.structs_map.get(name)
			else {
				return Err(format!(
					"No external struct by name `{name}` is registered."
				).into())
			};
		Ok(*d)
	}


	/// Registers an external struct to be used within the Qu langauge.
	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		let r_struct = QuStruct::new(
			<S as QuRegisterStruct>::name(),
			&<S as QuRegisterStruct>::register_fns,
			size_of::<S>(),
		);
		
		self.structs_map.insert(
			<S as QuRegisterStruct>::name().to_owned(),
			ClassId::new(self.structs.len()),
		);
		self.structs.push(r_struct);

	}

}


#[derive(Clone, Copy, Debug, Default, Hash, Eq, Ord, PartialEq, PartialOrd)]
pub struct ClassId(pub usize);
impl ClassId {
	pub fn new(index:usize) -> Self {
		Self(index)
	}
} impl From<usize> for ClassId {
	fn from(index: usize) -> Self {
		Self(index)
	}
}


impl From<ClassId> for u8 {
	fn from(v:ClassId) -> Self {
		v.0 as u8
	}
} impl From<ClassId> for usize {
	fn from(v:ClassId) -> Self {
		v.0 as usize
	}
}


#[derive(Clone)]
pub struct QuStruct {
	pub constants_map: HashMap<String, ConstantId>,
	pub external_functions_map: HashMap<FunctionIdentity, ExternalFunctionId>,
	pub functions_map: HashMap<FunctionIdentity, FunctionId>,
	pub function_groups_map: HashMap<String, FunctionGroupId>,
	
	pub name: String,
	pub register_fn: &'static dyn Fn() -> Vec<ExternalFunction>,
	/// The size of the struct in bytes.
	pub size: u8,

} impl QuStruct {
	pub fn new(
		name:impl Into<String>,
		fn_registerer:&'static dyn Fn() -> Vec<ExternalFunction>,
		size:usize,
	) -> Self {
		let name = name.into();
		assert!(size < u8::MAX as usize);
		Self {
			name,
			register_fn: fn_registerer,
			size: size as u8,
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			function_groups_map: Default::default(),
		}
	}
} impl Debug for QuStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QuStruct")
			.field("name", &self.name)
			.finish()
    }
} impl Default for QuStruct {
    fn default() -> Self {
        Self {
			constants_map: Default::default(),
			external_functions_map: Default::default(),
			functions_map: Default::default(),
			name: Default::default(),
			register_fn: &|| {vec![]},
			size: Default::default(),
			function_groups_map: Default::default(),
		}
    }
}

