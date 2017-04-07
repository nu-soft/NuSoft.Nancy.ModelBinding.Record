$root = (split-path -parent $MyInvocation.MyCommand.Definition) + '\..'
$version = [System.Reflection.Assembly]::LoadFile("$root\NuSoft.Nancy.ModelBinding.FSharp\bin\Release\NuSoft.Nancy.ModelBinding.FSharp.dll").GetName().Version
$versionStr = "{0}.{1}.{2}" -f ($version.Major, $version.Minor, $version.Build)

Write-Host "Setting .nuspec version tag to $versionStr"

$content = (Get-Content $root\NuGet\NuSoft.Nancy.ModelBinding.FSharp.nuspec) 
$content = $content -replace '\$version\$',$versionStr

$content | Out-File $root\nuget\NuSoft.Nancy.ModelBinding.FSharp.compiled.nuspec

& $root\NuGet\NuGet.exe pack $root\nuget\NuSoft.Nancy.ModelBinding.FSharp.compiled.nuspec