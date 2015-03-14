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

with Natools.Web.Escapes;

package body Natools.Web.Filters.Text_Blocks is

   Begin_Paragraph : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('p'),
         3 => Character'Pos ('>'));

   End_Paragraph : constant Ada.Streams.Stream_Element_Array
     := (1 => Character'Pos ('<'),
         2 => Character'Pos ('/'),
         3 => Character'Pos ('p'),
         4 => Character'Pos ('>'));

   function Newline (Encoding : Newline_Encoding)
     return Ada.Streams.Stream_Element_Array;
      --  Return the newline representation assicated with Encoding


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   function Newline (Encoding : Newline_Encoding)
     return Ada.Streams.Stream_Element_Array is
   begin
      case Encoding is
         when CR    => return (1 => 13);
         when LF    => return (1 => 10);
         when CR_LF => return (1 => 13, 2 => 10);
      end case;
   end Newline;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;

      type Automaton_State is
        (CR_Read, LF_Read, Newline_Read, Between_Paragraphs, In_Text);

      State : Automaton_State := Between_Paragraphs;
      Index : Ada.Streams.Stream_Element_Offset := Data'First;
   begin
      while Index in Data'Range loop
         case Data (Index) is
            when 13 =>
               case State is
                  when CR_Read | Newline_Read =>
                     Output.Write (End_Paragraph);
                     Output.Write (Newline (Object.Encoding));
                     State := Between_Paragraphs;

                  when LF_Read =>
                     State := Newline_Read;

                  when Between_Paragraphs =>
                     null;

                  when In_Text =>
                     State := CR_Read;
               end case;
               Index := Index + 1;

            when 10 =>
               case State is
                  when LF_Read | Newline_Read =>
                     Output.Write (End_Paragraph);
                     State := Between_Paragraphs;

                  when CR_Read =>
                     State := Newline_Read;

                  when Between_Paragraphs =>
                     null;

                  when In_Text =>
                     State := LF_Read;
               end case;
               Index := Index + 1;

            when others =>
               case State is
                  when CR_Read | LF_Read | Newline_Read =>
                     Output.Write (Newline (Object.Encoding));

                  when Between_Paragraphs =>
                     Output.Write (Begin_Paragraph);

                  when In_Text =>
                     null;
               end case;

               State := In_Text;

               declare
                  Next : Ada.Streams.Stream_Element_Offset := Index + 1;
               begin
                  while Next in Data'Range
                    and then Data (Next) not in 10 | 13
                  loop
                     Next := Next + 1;
                  end loop;

                  Escapes.Write
                    (Output, Data (Index .. Next - 1), Escapes.HTML_Body);
                  Index := Next;
               end;
         end case;
      end loop;

      case State is
         when Between_Paragraphs =>
            null;

         when CR_Read | LF_Read | Newline_Read | In_Text =>
            Output.Write (End_Paragraph);
            Output.Write (Newline (Object.Encoding));
      end case;
   end Apply;


   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Filters.Filter'Class
   is
      use type S_Expressions.Events.Event;
      Result : Filter;
   begin
      if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
         begin
            Result.Encoding := Newline_Encoding'Value
              (S_Expressions.To_String (Arguments.Current_Atom));
         exception
            when Constraint_Error => null;
         end;
      end if;

      return Result;
   end Create;


   not overriding procedure Set_Newline_Encoding
     (Object : in out Filter;
      Encoding : in Newline_Encoding) is
   begin
      Object.Encoding := Encoding;
   end Set_Newline_Encoding;

end Natools.Web.Filters.Text_Blocks;
