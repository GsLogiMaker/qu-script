
use std::collections::HashMap;
use std::fmt::Debug;
use std::mem::size_of;

use crate::ExternalFunction;
use crate::QuMsg;
use crate::QuRegisterStruct;


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct QuFunctionId(pub usize);
impl QuFunctionId {

	pub fn new(index:usize) -> Self {
		Self(index)
	}

} impl From<usize> for QuFunctionId {

	fn from(v:usize) -> Self {
		QuFunctionId(v)
	}

}


impl From<QuFunctionId> for u8 {
	fn from(v:QuFunctionId) -> Self {
		v.0 as u8
	}
}


#[derive(Default)]
pub struct QuRegistered {
	pub fns:Vec<ExternalFunction>,
	pub structs:Vec<QuStruct>,
	pub structs_map:HashMap<String, QuStructId>,
} impl QuRegistered {

	fn new() -> Self {
		Self {
			fns: Vec::default(),
			structs: Vec::default(),
			structs_map: HashMap::default(),
		}
	}
	

	pub fn get_fn_data_by_id(&self, fn_id:QuFunctionId
	) -> Result<&ExternalFunction, QuMsg>{
		match self.fns.get(fn_id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external function by id `{:?}` is registered.",
				fn_id
			).into()),
		}
	}


	pub fn get_fn_id(&self, fn_name:&str, struct_name:&str
	) -> Result<QuFunctionId, QuMsg> {
		let s_data = self.get_struct(struct_name)?;
		Ok(s_data.get_fn_id(fn_name)?)
	}


	pub fn get_struct(&self, name:&str
	) -> Result<&QuStruct, QuMsg>{
		Ok(self.get_struct_by_id(self.get_struct_id(name)?)?)
	}


	pub fn get_struct_by_id(&self, id:QuStructId
	) -> Result<&QuStruct, QuMsg>{
		match self.structs.get(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_by_id_mut(&mut self, id:QuStructId
	) -> Result<&mut QuStruct, QuMsg>{
		match self.structs.get_mut(id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external struct by id `{:?}` is registered.",
				id
			).into()),
		}
	}


	pub fn get_struct_id(&self, name:&str
	) -> Result<QuStructId, QuMsg>{
		let Some(d) = self.structs_map.get(name)
			else {
				return Err(format!(
					"No external struct by name `{name}` is registered."
				).into())
			};
		Ok(*d)
	}


	/// Registers the functions of the previously registered structs to be used
	/// in the Qu language.
	/// 
	/// # Note
	/// 
	/// A struct must be registered with [`Qu::register_struct`] before its
	/// functions can be registered.
	pub fn register_fns(&mut self) {
		let mut registrations = vec![];
		for s in self.structs.iter() {
			registrations.push(s.register_fn);
		}
		for registration in registrations {
			for external_function in (registration)() {
				self.register_fn(external_function);
			}
		}
	}


	/// Registers an [`ExternalFunction`] to Qu.
	pub fn register_fn(
		&mut self, external_function:ExternalFunction,
	) -> Result<(), QuMsg> {
		let new_function_id = QuFunctionId::new(self.fns.len());

		// Associate function as a method of its first perameter
		let struct_id = *external_function
			.parameters
			.get(0)
			.unwrap_or(&QuStructId::from(0));
		let struct_data = self.get_struct_by_id_mut(struct_id)?;
		struct_data.fns_map.insert(external_function.name.clone(), new_function_id);

		// Add function to list
		self.fns.push(external_function);

		Ok(())
	}


	/// Registers an external struct to be used within the Qu langauge.
	/// 
	/// # Note
	/// 
	/// Does not register the methods of the struct. Call
	/// [`Qu::register_struct`] to register the methods of all registered
	/// structs.
	/// 
	/// # Example
	/// 
	/// ```
	/// use qu::Qu;
	/// use qu::QuRegisterStruct;
	/// 
	/// struct MyStruct();
	/// impl QuRegisterStruct for MyStruct {
	/// 	fn get_name() -> String {
	/// 		"MyStruct".into()
	/// 	}
	/// }
	/// 
	/// let mut qu = Qu::new();
	/// 
	/// qu.register_struct::<MyStruct>();
	/// ```
	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		let r_struct = QuStruct::new(
			<S as QuRegisterStruct>::get_name(),
			&<S as QuRegisterStruct>::register_fns,
			size_of::<S>(),
		);
		
		self.structs_map.insert(
			<S as QuRegisterStruct>::get_name().to_owned(),
			QuStructId::new(self.structs.len()),
		);
		self.structs.push(r_struct);

	}

}


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct QuStructId(pub usize);
impl QuStructId {

	pub fn new(index:usize) -> Self {
		Self(index)
	}

} impl From<usize> for QuStructId {

	fn from(index: usize) -> Self {
		Self(index)
	}

}


impl From<QuStructId> for u8 {
	fn from(v:QuStructId) -> Self {
		v.0 as u8
	}
}


#[derive(Clone)]
pub struct QuStruct {
	pub name: String,
	pub fns_map: HashMap<String, QuFunctionId>,
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
			fns_map: HashMap::default(),
			register_fn: fn_registerer,
			size: size as u8,
		}
	}


	pub fn get_fn_id(&self, name:&str) -> Result<QuFunctionId, QuMsg> {
		let Some(fn_id) = self.fns_map.get(name) else {
			return Err(format!(
				"External struct `{}` has no registered function by name `{}`",
				self.name,
				name,
			).into())
		};

		Ok(*fn_id)
	}


	pub fn has_fn(&self, fn_name:&str) -> bool {
		self.fns_map.contains_key(fn_name)
	}

} impl Debug for QuStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("QuStruct")
			.field("name", &self.name)
			.field("fns_map", &self.fns_map)
			.finish()
    }
}

