with Text_IO;             use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

with PNM_Reader;  use PNM_Reader;

procedure PNMRead is
   
begin
   
   Put_Line ("PNM Read");
   
   declare
      R : Raster_Type(10,20);
   begin
      for I in R.Raster'Range(1) loop
         for J in R.Raster'Range(2) loop
            R.Raster (I,J) := 0;
         end loop;
      end loop;
      
      declare
         PR : Pixel_Raster_Type16 := Get_Raster (R);
      begin
         for I in PR'Range(1) loop
            for J in PR'Range(2) loop
               PR (I,J) := 1;
            end loop;
         end loop;
         
         Put (Integer (R.Raster (1,1)));
         New_Line;
         Put (Integer (PR (1,1)));
         New_Line;
      end;
   end;
   
end PNMRead;
