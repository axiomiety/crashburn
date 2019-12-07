'use strict';

const convertHexStringToRgba = (str) => {
    const decimalValue = parseInt(str,16);
    const paletteWidth = 47;
    let val = decimalValue*paletteWidth;
    const color = {r: 0, g: 0, b: 0};
    if (val > 255*2) {
      color.b = Math.round(val - 255*2);
      color.r, color.g = 255;
    } else if (val > 255) {
      color.g = Math.round(val - 255);
      color.r = 255;
    } else {
      color.r = Math.round(val);
    }    
    return `rgba(${color.r}, ${color.g}, ${color.b}, 1)`;
    //return '#FF0000';
  }
  
  const formatByteStringToHexNumbersArray = (str) => {
    const ret = [];
    if (str.length == 1) {
      ret.push('0', str);
    } else {
      ret.push(str[0], str[1]);
    }
    return ret;
  }

  exports.convertHexStringToRgba = convertHexStringToRgba;
  exports.formatByteStringToHexNumbersArray = formatByteStringToHexNumbersArray;