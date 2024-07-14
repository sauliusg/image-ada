with Text_IO;             use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;

with PNM_Reader;

procedure PNMRead is
   
   type Pixel_Type16 is mod 2**16;
   
   package PNM_Reader16 is new PNM_Reader (Pixel_Type16);
   use PNM_Reader16;
   
   subtype Pixel_Raster_Type16 is PNM_Reader16.Pixel_Raster_Type;
   
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
   
   if Argument_Count > 0 then
      New_Line;
   end if;
   
   for I in 1 .. Argument_Count loop
      declare
         Image : PNM_Image_Type;
      begin
         Load_Raster (Argument (I), Image);
      end;
      if I < Argument_Count then
         New_Line;
      end if;
   end loop;
   
end PNMRead;
