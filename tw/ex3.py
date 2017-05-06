import re
import io

def REGEXs():
  return {
  re.compile('^(\w+) is ([IVXLCDM])$')                        : update_iu_mapping
  ,re.compile('^(.+) ([A-Z][a-z]+) is (\d+) Credits$')        : update_unit_prc_mapping
  ,re.compile('^how much is (.+) \?$')                        : how_much
  ,re.compile('^how many Credits is (.+) ([A-Z][a-z]+) \?$')  : how_many
  }

# TODO: bunch the roman functions together in a class?

def _is_larger(a, b):
  ''' returns True if a has a larger decimal value than b '''
  ordering = 'IVXLCDM'
  return ordering.index(a) > ordering.index(b)

def roman_to_decimal(roman):
  r_to_a = {'M': 1000, 'D': 500, 'C': 100, 'L': 50, 'X': 10, 'V': 5, 'I': 1}
  values = []
  for idx, r in enumerate(roman):
    val = r_to_a[r]
    if idx < len(roman)-1 and _is_larger(roman[idx+1], r):
      val *= -1
    values.append(val)
  return sum(values)

def _iu_to_roman(iu, mapping):
  return ''.join(mapping[o] for o in iu.split())

def iu_to_decimal(iu, mapping_iu_roman):
  return roman_to_decimal(_iu_to_roman(iu, mapping_iu_roman))

def update_iu_mapping(iu, roman, mapping):
  mapping[iu] = roman

def update_unit_prc_mapping(iu_qty, material, num_credits, mapping):
  numeric_qty = iu_to_decimal(iu_qty, mapping)
  mapping[material] =  int(num_credits)/numeric_qty

def how_much(tokens, mapping):
  return '{iu} is {r}'.format(iu=tokens, r=iu_to_decimal(tokens, mapping))

def how_many(iu, material, mapping):
  dec_amount = iu_to_decimal(iu, mapping)
  unit_prc = mapping[material]
  # we use :g to remove unnecessary zeros
  return '{iu} {material} is {credits:g} Credits'.format(iu=iu, material=material, credits=unit_prc*dec_amount)

def parse_line(line, mapping):
  for pattern, fn in REGEXs().items():
    m = pattern.match(line)
    if m:
      return fn(*m.groups(), mapping=mapping)
  return 'I have no idea what you are talking about'

def parse(f, output=None):
  ''' f is a stream '''
  mapping = {}
  output = output or io.StringIO()
  for line in f:
    o = parse_line(line, mapping)
    if o:
      output.write(o)
      output.write('\n')

  return output

if __name__ == '__main__':
  import sys
  if len(sys.argv) != 2:
    print('scriptname <filename>')
    sys.exit()
  fname = sys.argv[1]
  with open(fname) as f:
    parse(f, sys.stdout)
