function c0(p0, p1, p2) {
  let v3;
  v3 = {
    f0: "John",
    f1: 10
  };
  let v4;
  v4 = v3;
  v4.f1 = 55;
  v3 = {
    f0: "Maria",
    f1: v3.f1
  };
  return v3.f1;
}
