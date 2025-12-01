function component1(param0 = "Pedro comeu pastel"){
  const out = {prop0:param0}
  (out.element = document.createElement("h1")).textContent = param0;
  return out;
}
function main(){
  return component1("edro comeu pastel",)
}
main();