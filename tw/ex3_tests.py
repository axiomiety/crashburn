
import unittest
import io
import ex3

class ExTest(unittest.TestCase):

  def test_roman_is_larger(self):
    self.assertTrue(ex3._is_larger('M','C'))
    self.assertFalse(ex3._is_larger('V','L'))
    self.assertFalse(ex3._is_larger('D','D'))

  def test_roman_to_decimal(self):

    tests = [('IV', 4),('III', 3)]
    for (roman, dec) in tests:
      self.assertEqual(ex3.roman_to_decimal(roman), dec)

  def test_iu_to_roman(self):
    mapping = {'glob': 'I', 'phish': 'V'}
    self.assertEqual(ex3._iu_to_roman('glob glob', mapping), 'II')
    self.assertEqual(ex3._iu_to_roman('phish glob', mapping), 'VI')
    self.assertEqual(ex3._iu_to_roman('phish', mapping), 'V')

  def test_qty(self):
    mapping = {'glob': 'I', 'phish': 'V'}
    iu_qty, material, num_credits = 'glob glob', 'Gold', '2'
    ex3.update_unit_prc_mapping(iu_qty, material, num_credits, mapping)
    self.assertEqual(mapping['Gold'], 1)

  def test_parse_line(self):
    m = {}
    ex3.parse_line('prok is V', m)
    self.assertEqual(m, {'prok': 'V'})
    ex3.parse_line('prok Gold is 20 Credits', m)
    self.assertEqual(m, {'prok': 'V', 'Gold': 4})

  def test_how_much(self):
    m = {'pish': 'X', 'tegj': 'L', 'glob': 'I' }
    self.assertEqual(ex3.how_much('pish tegj glob glob', m), 'pish tegj glob glob is 42')
  
  def test_how_many(self):
    m = {'prok': 'V', 'tegj': 'L', 'glob': 'I', 'Silver': 17}
    self.assertEqual(ex3.how_many('glob prok', 'Silver', m)
                                , 'glob prok Silver is 68 Credits')

  def test_all(self):
    s = '''glob is I
prok is V
pish is X
tegj is L
glob glob Silver is 34 Credits
glob prok Gold is 57800 Credits
pish pish Iron is 3910 Credits
how much is pish tegj glob glob ?
how many Credits is glob prok Silver ?
how many Credits is glob prok Gold ?
how many Credits is glob prok Iron ?
how much wood could a woodchuck chuck if a woodchuck could chuck wood ?'''
    o = ex3.parse(io.StringIO(s))
    o.seek(0)
    expected = '''pish tegj glob glob is 42
glob prok Silver is 68 Credits
glob prok Gold is 57800 Credits
glob prok Iron is 782 Credits
I have no idea what you are talking about
'''
    self.assertEqual(''.join(o.readlines()), expected)
    o.close()


if __name__ == '__main__':
  unittest.main()
