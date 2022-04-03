
Convert swtbug.bin ROm to swtbug.s19

   srec_cat.exe swtbug.bin -binary -offset 57344 -Output sw.s19 -Motorola


Convert b.s19 ROm to b.bin

   srec_cat b.s19 -o b.bin -binary