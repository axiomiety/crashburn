exports.flatten = function (arr) {
  return arr.reduce((acc,val) => acc.concat(val));
}

exports.every = function(arr, pred) {
  for (var v in arr) {
    if (pred(arr[v])) { continue; }
    return false;
  }
  return true;
}

exports.some = function(arr, pred) {
  for (var v in arr) {
    if (pred(arr[v])) { return true; }
    continue;
  }
  return false;
}
