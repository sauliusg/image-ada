with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- https://stackoverflow.com/questions/62348509/ada-program-to-detect-an-end-of-line
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

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
      if Img.Raster /= null then
         Img.Raster.Ref_Count := Img.Raster.Ref_Count + 1;
      end if;
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
   
   function Get_Binary_Stream_Value
     (
      Input : in Stream_Access;
      Max_Val : in Integer
     ) return Integer is
      Value : Integer;
      Char1 : Character;
      Char2 : Character;
   begin
      Character'Read (Input, Char1);
      Value := Character'Pos (Char1);
      if Max_Val > 255 then
         Character'Read (Input, Char2);
         Value := Value * 256 + Character'Pos (Char2);
      end if;
      return Value;
   end;
   
   procedure PNM_Read_Grayscale_Binary_Raster
     (
      File : in File_Type;
      Max_Val : in Integer;
      R : in out PNM_Image_Type
     ) is
      -- https://stackoverflow.com/questions/62348509/ada-program-to-detect-an-end-of-line:
      Input : Stream_Access := Stream (File);
      Value : Integer;
   begin
      for I in R.Raster.Raster'Range (1) loop
         for J in R.Raster.Raster'Range (2) loop
            Value := Get_Binary_Stream_Value (Input, Max_Val);
            R.Raster.Raster (I, J) := Pixel_Type (Value);
         end loop;
      end loop;
   end;         
   
   procedure PNM_Read_Grayscale_ASCII_Raster
     (
      File : in File_Type;
      R : in out PNM_Image_Type
     ) is
      Value : Integer;
   begin
      for I in R.Raster.Raster'Range (1) loop
         for J in R.Raster.Raster'Range (2) loop
            Get (File, Value);
            R.Raster.Raster (I, J) := Pixel_Type (Value);
         end loop;
      end loop;
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
               Get (Line (Pos + 1 .. Line'Last), W, Pos);
               Get (Line (Pos + 1 .. Line'Last), H, Pos);
               exit;
            end if;
         end;
      end loop;
      
      -- For grayscale formats (P2 or P5), as opposed to single bit
      -- maps (P1 or P4), a maximum pixel value is also given:
      if Format = P2_FORMAT or else Format = P5_FORMAT then
         declare
            Line : String := Get_Line (File);
            Dummy : Natural;
         begin
            Get (Line, Max_Val, Dummy);
         end;
      end if;
      
      R.Raster := new Raster_Type (H, W);
      R.Format := Format;
      R.MaxVal := Max_Val;
      
      -- load the raster:
      if Format = P1_FORMAT or else Format = P2_FORMAT then
         PNM_Read_Grayscale_ASCII_Raster (File, R);
      elsif Format = P5_FORMAT then
         PNM_Read_Grayscale_Binary_Raster (File, Max_Val, R);
      else
         raise FORMAT_ERROR with
           "format '" & Format'Image & "' is not yet supported";
      end if;
   end;

   procedure Load_Raster (File : in File_Type; R : out PNM_Image_Type) is
      PNM_Signature : String := Get_Line (File);
      PNM_Format : PNM_Format_Type := Get_Image_Format (PNM_Signature);
   begin
      case PNM_Format is
         when P1_FORMAT | P2_FORMAT | P5_FORMAT =>
            Read_Grayscale_Raster (File, PNM_Format, R);
         when others =>
            raise FORMAT_ERROR with
              "unrecognised format " & PNM_Format'Image;
      end case;
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
