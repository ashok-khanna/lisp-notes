REM Script to remove development cruft from this directory

for %%f in (plugin-example prepare-fm-plugin-tools DriverDLL doc .) do del %%f\*.bak, %%f\*.ofasl, %%f\*.*~ /s /q
