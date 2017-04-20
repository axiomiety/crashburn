
function range(start, stop, step=1) {
  var arr = [];
  var count = 0;
  var max_steps = Math.abs(stop-start)/Math.abs(step);
  while (count <= max_steps) {
    arr.push(start + step*count);
    count += 1;
  }
  return arr;
}

exports.sum = function(arr) {
  return arr.reduce(function(acc,val) {return acc+val;});
}

exports.range = range;

exports.reverseArray = function(arr) {
	var rev = Array();
	for (var i=0; i<arr.length; i+=1) {
		rev.unshift(arr[i]);
	}
	return rev;
}

exports.reverseArrayInPlace = function(arr) {
	for (var i=0; i<Math.floor(arr.length/2); i+=1) {
		var tmp = arr[i];
		arr[i] = arr[arr.length-1-i];
		arr[arr.length-1-i] = tmp;
	}
}

function prepend(li, val) {
	if (li == null) { return {value: val, rest: null};}
	return {value: val, rest: li}
}

exports.arrayToList = function(arr) {
	var li = null;
	for (var i=arr.length-1; i>=0; i-=1) {
		li = prepend(li, arr[i]);
	}
	return li;
}

exports.listToArray = function(li) {
	var arr = Array();

	while (li.rest != null) {
		arr.push(li.value);
		li = li.rest;
	}
	arr.push(li.value);
	return arr;
}

function nth(li,n) {
	if (n==0) {return li.value};
	if (li.rest != null) {return nth(li.rest, n-1);}
	return undefined;
}

exports.nth = nth;

function deepEqual(obj1, obj2) {
  if (obj1 === obj2) { return true; }
  
  if (typeof obj1 == 'object' && typeof obj2 == 'object') {
    props1 = Object.keys(obj1);
    props2 = Object.keys(obj2);
    if (props1.length != props2.length) { return false; }
    if (!props1.every(p => props2.includes(p))) { return false; }
    return props1.every(p => deepEqual(obj1[p],obj2[p]));
  }

  return false;
}

exports.deepEqual = deepEqual;
