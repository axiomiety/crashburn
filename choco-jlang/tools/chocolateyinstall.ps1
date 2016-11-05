$ErrorActionPreference = 'Stop';

$packageName= 'jlang'
$toolsDir   = "$(Split-Path -parent $MyInvocation.MyCommand.Definition)"
$url        = 'http://www.jsoftware.com/download/j805/install/j805_win32.exe'
$url64      = 'http://www.jsoftware.com/download/j805/install/j805_win64.exe'

$packageArgs = @{
  packageName   = $packageName
  unzipLocation = $toolsDir
  fileType      = 'EXE'
  url           = $url
  url64bit      = $url64

  softwareName  = 'jlang*'

  checksum      = '40E0445294E5F350F21D1E232B11A06D3925081EA20EE046106D8B9D38D8690D'
  checksumType  = 'sha256'
  checksum64    = '05DC1B2CEFEB5A96DF8B975DFDBB45423CD8DB17B7458A076C5BDFDA0A8DD05D'
  checksumType64= 'sha256'
  validExitCodes= @(0)
}

Install-ChocolateyZipPackage @packageArgs