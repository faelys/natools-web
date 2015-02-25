------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

package body Natools.Web.Escapes is

   --------------------
   -- Entity Strings --
   --------------------

   package Entities is
      Amp : constant S_Expressions.Atom
        := (1 => Character'Pos ('&'),
            2 => Character'Pos ('a'),
            3 => Character'Pos ('m'),
            4 => Character'Pos ('p'),
            5 => Character'Pos (';'));

      Apos : constant S_Expressions.Atom
        := (1 => Character'Pos ('&'),
            2 => Character'Pos ('a'),
            3 => Character'Pos ('p'),
            4 => Character'Pos ('o'),
            5 => Character'Pos ('s'),
            6 => Character'Pos (';'));

      Gt : constant S_Expressions.Atom
        := (1 => Character'Pos ('&'),
            2 => Character'Pos ('g'),
            3 => Character'Pos ('t'),
            4 => Character'Pos (';'));

      Lt : constant S_Expressions.Atom
        := (1 => Character'Pos ('&'),
            2 => Character'Pos ('l'),
            3 => Character'Pos ('t'),
            4 => Character'Pos (';'));

      Quot : constant S_Expressions.Atom
        := (1 => Character'Pos ('&'),
            2 => Character'Pos ('q'),
            3 => Character'Pos ('u'),
            4 => Character'Pos ('o'),
            5 => Character'Pos ('t'),
            6 => Character'Pos (';'));
   end Entities;



   ---------------------
   -- Counting Stream --
   ---------------------

   overriding procedure Read
     (Stream : in out Count_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream, Item, Last);
   begin
      raise Program_Error with "Count_Stream is write-only";
   end Read;


   overriding procedure Write
     (Stream : in out Count_Stream;
      Item : in Ada.Streams.Stream_Element_Array)
   is
      use type S_Expressions.Count;
   begin
      Stream.Count := Stream.Count + Item'Length;
   end Write;



   -----------------------
   -- Private Interface --
   -----------------------

   procedure Update_Set (Octets : in out Octet_Set; Set : in Escape_Set) is
   begin
      if Set.Gt_Lt then
         Octets (Character'Pos ('<')) := True;
         Octets (Character'Pos ('>')) := True;
      end if;

      if Set.Amp then
         Octets (Character'Pos ('&')) := True;
      end if;

      if Set.Apos then
         Octets (Character'Pos (''')) := True;
      end if;

      if Set.Quot then
         Octets (Character'Pos ('"')) := True;
      end if;
   end Update_Set;



   ----------------------
   -- Public Interface --
   ----------------------

   function Escaped_Length
     (Data : in S_Expressions.Atom;
      Set : in Escape_Set)
     return S_Expressions.Count
   is
      Counter : Count_Stream;
   begin
      Write (Counter, Data, Set);
      return Counter.Count;
   end Escaped_Length;


   procedure Write
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in S_Expressions.Atom;
      Set : in Escape_Set)
   is
      use type S_Expressions.Offset;
      I : S_Expressions.Offset;
      Escaped : Octet_Set := (others => False);
   begin
      Update_Set (Escaped, Set);
      I := Data'First;

      Main_Loop :
      loop
         declare
            First : constant S_Expressions.Offset := I;
         begin
            Process_Unescaped :
            while I in Data'Range and then not Escaped (Data (I)) loop
               I := I + 1;
            end loop Process_Unescaped;

            if I > First then
               Output.Write (Data (First .. I - 1));
            end if;
         end;

         exit Main_Loop when I not in Data'Range;

         Process_Escaped :
         while I in Data'Range and then Escaped (Data (I)) loop
            case Data (I) is
               when Character'Pos ('<') =>
                  Output.Write (Entities.Lt);

               when Character'Pos ('>') =>
                  Output.Write (Entities.Gt);

               when Character'Pos ('&') =>
                  Output.Write (Entities.Amp);

               when Character'Pos (''') =>
                  Output.Write (Entities.Apos);

               when Character'Pos ('"') =>
                  Output.Write (Entities.Quot);

               when others =>
                  raise Program_Error with "No escape for "
                    & Character'Image (Character'Val (Data (I)));
            end case;

            I := I + 1;
         end loop Process_Escaped;

         exit Main_Loop when I not in Data'Range;
      end loop Main_Loop;
   end Write;


   procedure Write
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Text : in String;
      Set : in Escape_Set) is
   begin
      Write (Output, S_Expressions.To_Atom (Text), Set);
   end Write;

end Natools.Web.Escapes;
