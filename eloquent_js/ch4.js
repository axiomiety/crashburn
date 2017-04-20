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
