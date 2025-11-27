function component0(A){
  const out = {prop0:A??5}
  return out;
}
function component1(A,B,C){
  const out = {prop0:A,prop1:B??22,prop2:C}
  return out;
}
function main(){
  return component1(5,12,[component0()])
}
main();