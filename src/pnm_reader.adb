with Text_IO;   use Text_IO;

with Ada.Unchecked_Deallocation;

package body PNM_Reader is
   
   procedure Free is new
     Ada.Unchecked_Deallocation (Raster_Type, Access_Raster);
   
   procedure Initialize (Img : in out PNM_Image_Type) is
   begin
      null;
   end;
   
   procedure Finalize (Img : in out PNM_Image_Type) is
   begin
      if Img.Raster /= null then
         Img.Raster.Ref_Count := Img.Raster.Ref_Count - 1;
         if Img.Raster.Ref_Count = 0 then
            Free (Img.Raster);
         end if;
      end if;
   end;
   
   procedure Adjust (Img : in out PNM_Image_Type) is
   begin
      Img.Raster.Ref_Count := Img.Raster.Ref_Count + 1;
   end;
   
   function Get_Raster (R : Raster_Type) return Pixel_Raster_Type16 is
   begin
      return R.Raster;
   end;
   
   procedure Load_Raster (F : in File_Type; R : out PNM_Image_Type ) is
      PNM_Signature : String := Get_Line (F);
   begin
      null;
   end;

end PNM_Reader;
