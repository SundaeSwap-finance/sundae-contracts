use types/order.{OrderDatum}

validator {
  fn spend(_d: OrderDatum, _r: Data, _ctx: Data) {
    True
  }
}
