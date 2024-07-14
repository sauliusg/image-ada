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
   
   function Get_Raster (R : Raster_Type) return Pixel_Raster_Type is
   begin
      return R.Raster;
   end;
   
   procedure Load_Raster (F : in File_Type; R : out PNM_Image_Type ) is
      PNM_Signature : String := Get_Line (F);
   begin
      null;
   end;
   
   function Get_Image_Format (Magic : String) return PNM_Format_Type
   is
   begin
      pragma Assert (Magic'Length = 2);
      if Magic = "P1" then
         return P1_FORMAT;
      elsif Magic = "P2" then
         return P2_FORMAT;
      elsif Magic = "P3" then
         return P3_FORMAT;
      elsif Magic = "P4" then
         return P4_FORMAT;
      elsif Magic = "P5" then
         return P5_FORMAT;
      elsif Magic = "P6" then
         return P6_FORMAT;
      else
         return UNKNOWN;
      end if;
   end;

end PNM_Reader;
