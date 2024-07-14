with Text_IO;          use Text_IO;
with Ada.Finalization; use Ada.Finalization;

generic
   type Pixel_Type is mod <>;
   
package PNM_Reader is
   
   type Pixel_Raster_Type is array (Natural range <>, Natural range <>)
     of Pixel_Type;
   
   type PNM_Format_Type is
     (
      UNKNOWN,
      P1_FORMAT, -- pbm image, magic number "P1", pixels 1 or 0, ASCII
      P2_FORMAT, -- pgm image, magic number "P2", integer grayscale pixels, ASCII
      P3_FORMAT, -- ppm image, magic number "P6", 2 x interer pixels, ASCII
      P4_FORMAT, -- pbm image, magic number "P4", 1 bit per pixel, binary
      P5_FORMAT, -- pgm image, magic number "P5", 1 or 2 bytes, max. value 65536, binary
      P6_FORMAT  -- ppm image, magic number "P6", 1 or 2 bytes/color, maxval 65536, bin
     );
   
   type Raster_Type (N, M : Natural) is record
      Ref_Count : Natural;
      Raster : Pixel_Raster_Type (1..N, 1..M);
   end record;
   
   type Access_Raster is access Raster_Type;
   
   type PNM_Image_Type is new Controlled with record
      Raster : Access_Raster;
   end record;
   
   function Get_Raster (R : Raster_Type) return Pixel_Raster_Type;
   
   procedure Load_Raster (F : File_Type; R : out PNM_Image_Type );
   
private
   
   procedure Initialize(Img : in out PNM_Image_Type);
   procedure Finalize(Img : in out PNM_Image_Type);
   procedure Adjust(Img : in out PNM_Image_Type);   

end PNM_Reader;
