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
   
end PNM_Reader;
