pub type ActionId = usize;

pub type Primitive = String; //Lisp code;

#[derive(Default, Debug, Clone)]
pub struct Action {
    pub primitive: Primitive,
    pub id: ActionId,
}


