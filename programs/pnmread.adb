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
   
   for I in 1 .. Argument_Count loop
      declare
         Image : PNM_Image_Type;
      begin
         Load_Raster (Argument (I), Image);
         Put_Line (Image.Format'Image (1 .. 2));
         Put (Image.Raster.M, 0);
         Put (" ");
         Put (Image.Raster.N, 0);
         New_Line;
         if Image.Format /= P1_FORMAT and then IMage.Format /= P4_Format then
            Put (Image.MaxVal, 0);
            New_Line;
         end if;
         for I in Image.Raster.Raster'Range (1) loop
            for J in Image.Raster.Raster'Range (2) loop
               Put (Integer (Image.Raster.Raster (I, J)), 4);
            end loop;
            New_Line;
         end loop;
      end;      
      if I < Argument_Count then
         New_Line;
      end if;
   end loop;
   
end PNMRead;
