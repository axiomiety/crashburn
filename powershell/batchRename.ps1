
Function batchRename{
    Param([string] $dir, [string] $basename, [switch] $dryRun)
    $count = 1
    Write-Host "Base directory: $dir"
    foreach ($file in Get-ChildItem $dir -File) {
        $new_name = "{0}{1}{2}" -f $basename, $count.ToString("00"), [System.IO.Path]::GetExtension($file)
        if ($dryRun) {
            Rename-Item $file.FullName -NewName $new_name -WhatIf
        }
        else {
            Rename-Item $file.FullName -NewName $new_name
        }
        $count += 1
    }
    $count -= 1 # since we incremented one too many
    Write-Host "Renamed $count file(s)"
}