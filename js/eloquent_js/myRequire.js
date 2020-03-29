function myRequire(path, reload=false) {
  
  var exports = {};
  if (path in myRequire._cache) {
    if (reload) exports = myRequire._cache[path];
    else return myRequire._cache[path];
  }
  
  const moduleCode = fs.readFileSync(path, 'utf8'); // we should handle errors!
  var moduleFunction = new Function("exports", moduleCode); // `exports` will be made available inside `moduleCode`
  moduleFunction(exports);
  myRequire._cache[path] = exports;

  fs.watch(path, (et, fname) => { console.log('change detected'); myRequire(path, true);})

  return exports;
}
myRequire._cache = {};

exports.myr = myRequire;
