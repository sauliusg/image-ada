with Text_IO;          use Text_IO;
with Ada.Finalization; use Ada.Finalization;

package PNM_Reader is
   
   type Pixel_Type16 is mod 2**16;
   
   type Pixel_Raster_Type16 is array (Natural range <>, Natural range <>)
     of Pixel_Type16;
   
   type Raster_Type (N, M : Natural) is record
      Ref_Count : Natural;
      Raster : Pixel_Raster_Type16 (1..N, 1..M);
   end record;
   
   type Access_Raster is access Raster_Type;
   
   type PNM_Image_Type is new Controlled with record
      Raster : Access_Raster;
   end record;
   
private
   
   procedure Initialize(Img : in out PNM_Image_Type);
   procedure Finalize(Img : in out PNM_Image_Type);
   procedure Adjust(Img : in out PNM_Image_Type);   

end PNM_Reader;
