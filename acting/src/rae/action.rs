pub type ActionId = usize;

pub struct Primitive {

}


pub struct Action {
    primitive: Primitive,
    id: ActionId,
}