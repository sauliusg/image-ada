pragma Ada_2022;

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
      
      if Format = P2_FORMAT or else Format = P5_FORMAT then
         declare
            Line : String := Get_Line (File);
            Dummy : Natural;
         begin
            Get (Line, Max_Val, Dummy);
         end;
      end if;
      
      Put_Line (">>> Max_Val =" & Max_Val'Image);
      
      R.Raster := new Raster_Type (H, W);
      
      -- load the raster:
      if Format = P1_FORMAT or else Format = P2_FORMAT then
         declare
            Value : Integer;
         begin
            for I in R.Raster.Raster'Range (1) loop
               for J in R.Raster.Raster'Range (2) loop
                  Get (File, Value);
                  R.Raster.Raster (I, J) := Pixel_Type (Value);
               end loop;
            end loop;
         end;
      elsif Format = P5_FORMAT then
         declare
            -- https://stackoverflow.com/questions/62348509/ada-program-to-detect-an-end-of-line:
            Input : Stream_Access := Stream (File);
            Value : Integer;
            Char1 : Character;
            Char2 : Character;
         begin
            for I in R.Raster.Raster'Range (1) loop
               for J in R.Raster.Raster'Range (2) loop
                  Character'Read (Input, Char1);
                  Value := Character'Pos (Char1);
                  Put_Line (">>> Value = " & Value'Image);
                  if Max_Val > 255 then
                     Character'Read (Input, Char2);
                     Value := Value * 256 + Character'Pos (Char2);
                     Put_Line (">>> Value = " & Value'Image);
                  end if;
                  R.Raster.Raster (I, J) := Pixel_Type (Value);
               end loop;
            end loop;
         end;         
      else
         raise FORMAT_ERROR with
           "format '" & Format'Image & "' is not yet supported";
      end if;
      
      for I in R.Raster.Raster'Range (1) loop
         for J in R.Raster.Raster'Range (2) loop
            Put (Integer (R.Raster.Raster (I, J)), 4);
         end loop;
         New_Line;
      end loop;
      
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
