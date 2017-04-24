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

function average(arr) {
  return arr.reduce((a,b) => (a+b)) / arr.length;
}

exports.avg = average;

const ANCESTRY_FILE = require('./ancestry.js');
var ancestry = JSON.parse(ANCESTRY_FILE);
var byName = {};
ancestry.forEach(person => byName[person.name] = person);

exports.mcad = function() {
  var diffs = new Array;
  ancestry.forEach(function(p) {
    if (byName[p.mother]) {
      diffs.push(p.born - byName[p.mother].born);
    };
  });
  return average(diffs);
}

function groupBy(arr, fn) {
  var grouping = {};
  for (var i in arr) {
    var key = fn(arr[i]);
    if (!(key in grouping)) { grouping[key] = new Array(); }
    grouping[key].push(arr[i]);
  };
  return grouping;
};

exports.gb = groupBy;

exports.le = function() {
  var g = groupBy(ancestry, function(p) {return Math.ceil(p.died/100);});
  return Object.keys(g).map(k => average(g[k].map(p => p.died-p.born)));
};
