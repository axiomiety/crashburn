const wish = require('wish');
//import convertHexStringToRgb from '../index.js';
const { convertHexStringToRgba } = require('../lib.js');

describe('utilities', function() {
    it('converts a hexadecimal string (0-f) to an RGB triplet', function() {
        let ret = convertHexStringToRgba('0');
        wish( 'rgba(0, 0, 0, 1)' === ret);
        ret = convertHexStringToRgba('1');
        console.log(ret);
        wish( 'rgba(47, 0, 0, 1)' === ret);
        ret = convertHexStringToRgba('6');
        console.log(ret);
        wish( 'rgba(255, 27, 0, 1)' === ret); 
    });
});