
use std::collections::HashMap;
use std::fmt::Debug;

use crate::QuMsg;
use crate::QuExtFnData;
use crate::QuRegisterStruct;


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


#[derive(Default)]
pub struct QuImports {
	pub fns:Vec<QuExtFnData>,
	pub fns_map:HashMap<String, QuExtFnId>,
	pub structs:Vec<QuStruct>,
	pub structs_map:HashMap<String, QuStructId>,
} impl QuImports {

	/// Constructs a new [`QuImports`].
	pub fn new() -> Self {
		Self {
			fns: Vec::default(),
			fns_map: HashMap::default(),
			structs: Vec::default(),
			structs_map: HashMap::default(),
		}
	}
	

	pub fn get_fn_data_by_id(&self, fn_id:QuExtFnId
	) -> Result<&QuExtFnData, QuMsg>{
		match self.fns.get(fn_id.0) {
			Some(v) => Ok(v),
			None => Err(format!(
				"No external function by id `{:?}` is registered.",
				fn_id
			).into()),
		}
	}


	pub fn get_fn_id(&self, fn_name:&str, struct_name:&str
	) -> Result<QuExtFnId, QuMsg> {
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
		let mut fn_datas
			= Vec::default();
		for s in self.structs.iter_mut() {
			for data
			in (s.register_fn)() {
				let None = s.fns_map.insert(
					data.0.clone(),
					fn_datas.len().into(),
				) else {
					panic!(
						"Can't define a function with name {} in struct {} because it already has a function with that name.",
						data.0, s.name,
					);
				};
				fn_datas.push(data);
			}
		}
		self.fns = fn_datas;
	}


	pub fn register_fn(&mut self, fn_data:QuExtFnData) {
		self.fns_map.insert(
			fn_data.0.clone(),
			QuExtFnId::new(self.fns.len()),
		);
		self.fns.push(fn_data);
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
	/// qu.import_struct::<MyStruct>();
	/// ```
	pub fn register_struct<S:QuRegisterStruct+'static>(&mut self) {
		let r_struct = QuStruct::new(
			<S as QuRegisterStruct>::get_name(),
			&<S as QuRegisterStruct>::register_fns
		);
		
		self.structs_map.insert(
			<S as QuRegisterStruct>::get_name().to_owned(),
			QuStructId::new(self.structs.len()),
		);
		self.structs.push(r_struct);

	}


	pub fn register_struct_fn(
		&mut self, struct_id:QuStructId, fn_data:QuExtFnData
	) -> Result<(), QuMsg> {
		let new_idx = self.fns.len();
		let s = self.get_struct_by_id_mut(struct_id)?;
		s.fns_map.insert(
			fn_data.0.clone(),
			QuExtFnId::new(new_idx),
		);
		self.fns.push(fn_data);
		Ok(())
	}

}


#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct QuStructId(pub usize);
impl QuStructId {

	pub fn new(index:usize) -> Self {
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
	pub name:String,
	pub fns_map:HashMap<String, QuExtFnId>,
	pub register_fn:&'static dyn Fn() -> Vec<QuExtFnData>,

} impl QuStruct {

	pub fn new(
		name:impl Into<String>,
		fn_registerer:&'static dyn Fn() -> Vec<QuExtFnData>,
	) -> Self {
		let name = name.into();
		Self {
			name,
			fns_map: HashMap::default(),
			register_fn: fn_registerer,
		}
	}


	pub fn get_fn_id(&self, name:&str) -> Result<QuExtFnId, QuMsg> {
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

