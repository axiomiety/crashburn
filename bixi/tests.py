import  unittest
import  listener

class DistanceMatrixTest(unittest.TestCase):
  pass

class ParserTest(unittest.TestCase):
  
  @classmethod
  def setUpClass(cls):
    s = '''<?xml version='1.0' encoding='UTF-8'?><stations lastUpdate="1388933822252" version="2.0"><station><id>1</id><name>Jarvis St/ Carleton St</name><terminalName>7055</terminalName><lastCommWithServer>1388933151006</lastCommWithServer><lat>43.66207</lat><long>-79.37617</long><installed>true</installed><locked>false</locked><installDate/><removalDate/><temporary>false</temporary><public>true</public><nbBikes>8</nbBikes><nbEmptyDocks>7</nbEmptyDocks><latestUpdateTime>1388899793575</latestUpdateTime></station><station><id>5</id><name>Church St/ Alexander St</name><terminalName>7044</terminalName><lastCommWithServer>1388933649708</lastCommWithServer><lat>43.6636</lat><long>-79.3806</long><installed>true</installed><locked>false</locked><installDate/><removalDate/><temporary>false</temporary><public>true</public><nbBikes>3</nbBikes><nbEmptyDocks>12</nbEmptyDocks><latestUpdateTime>1388903405786</latestUpdateTime></station></stations>'''
    from xml.etree import ElementTree
    cls.xml = ElementTree.fromstring(s)
    unittest.TestCase.setUpClass()

  def test_jsonEquivalent(self):
    j = listener.xml2json(self.xml)
    expected = '''{"1388933822252": [{"avail_bikes": 8, "empty_docks": 7, "id": "1", "lat": 43.66207, "long": -79.37617, "name": "Jarvis St/ Carleton St", "updatets": 1388899793575}, {"avail_bikes": 3, "empty_docks": 12, "id": "5", "lat": 43.6636, "long": -79.3806, "name": "Church St/ Alexander St", "updatets": 1388903405786}]}'''
    self.assertEqual(expected, j)

if __name__ == '__main__':
  unittest.main()
