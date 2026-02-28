function c0() {
  ;
}
function c1() {
  let v0;
  v0 = {
    f0: "123",
    f1: 123
  };
  let v1;
  v1 = v0;
  v1.f0 = "Jorge";
  let v2;
  v2 = v0;
  v2.f1 = 255;
  v0 = {
    f0: "jorge",
    f1: v0.f18446744073709551615 + 3
  };
  return c0();
}
