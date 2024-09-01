use types/order.{OrderDatum}

validator documentation {
  spend(_d: Option<OrderDatum>, _r: Data, _o: Data, _transaction: Data) {
    True
  }

  else(_) {
    fail
  }
}
