with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

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
   
   procedure Load_Raster (Name : in String; R : out PNM_Image_Type) is
      File : File_Type;
   begin
      Open (File, In_File, Name);
      Load_Raster (File, R);
      Close (File);
   end;
   
   procedure Read_Grayscale_Raster
     (
      File : in File_Type;
      Format : in PNM_Format_Type;
      R : out PNM_Image_Type
     ) is
      W, H : Positive;
      Max_Val : Positive := 1;
   begin
      while not End_Of_File (File) loop
         declare
            Line : String := Get_Line (File);
            Pos : Natural := 0;
         begin
            if Line'Length > 1 and then Line (1) /= '#' then
               Put_Line (">>> Pos =" & Pos'Image);
               Get (Line (Pos + 1 .. Line'Last), W, Pos);
               Put_Line (">>> Pos =" & Pos'Image);
               Get (Line (Pos + 1 .. Line'Last), H, Pos);
               Put_Line (">>> Pos =" & Pos'Image);
               exit;
            end if;
         end;
      end loop;
      Put_Line (">>> W =" & W'Image);
      Put_Line (">>> H =" & H'Image);
   end;

   
   procedure Load_Raster (File : in File_Type; R : out PNM_Image_Type) is
      PNM_Signature : String := Get_Line (File);
      PNM_Format : PNM_Format_Type := Get_Image_Format (PNM_Signature);
   begin
      case PNM_Format is
         when P1_FORMAT | P2_FORMAT =>
            Read_Grayscale_Raster (File, PNM_Format, R);
         when others =>
            raise FORMAT_ERROR with
              "unrecognised format " & PNM_Format'Image;
      end case;
      
      Put_Line (">>> " & PNM_Format'Image);
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
