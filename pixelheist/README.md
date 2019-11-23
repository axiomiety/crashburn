
## Parsing the file

 * Assuming uncompressed (we can look at compression later, but should be irrelevant from a display/parsing perspective)
 * If the file is 16958 bytes, we will require 16958*2 squares
 * That's 33916
 * Assume a 1200x800 canvas - that's 960,000 squares
 * If we assume squares of 4x4 pixels, that means we can show 60,000 squares on that canvas. 5x5 pixels, that's 38400
 * We need to think about paging at some point!
 * From a boundary perspective, we could have the user input the file length
 * We need to create a palette - e.g. 16bit
 * The smaller the palette, the less likely compression will mess things up. If we had a 256bit palette that would greatly speed things up, but it probably won't be as accurate


TODO:
 * map each hex character to an equally space colour in the RGB spectrum
 * iterate through a file and draw 'em squares