------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
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

with Natools.Web.Filters.Pass_Through;

package body Natools.Web.Filters.Text_Replacement is

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
   is
      use type S_Expressions.Offset;
      use type S_Expressions.Atom;

      Last_Interesting : constant S_Expressions.Offset
        := Data'Last - Object.Pattern_Length;
      Index : S_Expressions.Offset := Data'First;
      Anchor : S_Expressions.Offset;
   begin
      Main_Loop :
      loop
         Anchor := Index;

         Non_Matching :
         while Index in Data'First .. Last_Interesting
           and then Data (Index .. Index + Object.Pattern_Length - 1)
              /= Object.Pattern
         loop
            Index := Index + 1;
         end loop Non_Matching;

         if Index not in Data'First .. Last_Interesting then
            Output.Write (Data (Anchor .. Data'Last));
            exit Main_Loop;
         elsif Index > Anchor then
            Output.Write (Data (Anchor .. Index - 1));
         end if;

         Output.Write (Object.Replacement);
         Index := Index + Object.Pattern_Length;
      end loop Main_Loop;
   end Apply;


   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Filters.Filter'Class
   is
      use type Natools.S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return Pass_Through.Create (Arguments);
      end if;

      declare
         Pattern : constant S_Expressions.Atom := Arguments.Current_Atom;
         Event : Natools.S_Expressions.Events.Event;
      begin
         Arguments.Next (Event);

         if Event /= S_Expressions.Events.Add_Atom then
            return Pass_Through.Create (Arguments);
         end if;

         declare
            Replacement : constant S_Expressions.Atom
              := Arguments.Current_Atom;
         begin
            return Filter'
              (Pattern_Length => Pattern'Length,
               Replacement_Length => Replacement'Length,
               Pattern => Pattern,
               Replacement => Replacement);
         end;
      end;
   end Create;

end Natools.Web.Filters.Text_Replacement;
