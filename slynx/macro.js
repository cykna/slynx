function component0(param0 = "Pedro comeu pastel",param1 = 5,param2 = param1,param3){
  const out = {prop0:param0,prop1:param1,prop2:param2,prop3:param3}
  (out.element = document.createElement("h1")).textContent = param0;
  return out;
}
function main(){
  return component0("Papel pedra tesora",)
}
main();