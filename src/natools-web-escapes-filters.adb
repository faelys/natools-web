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

package body Natools.Web.Escapes.Filters is

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array) is
   begin
      Write (Output, Data, Object.Set);
   end Apply;


   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Web.Filters.Filter'Class
   is
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event := Arguments.Current_Event;
      Set : Octet_Set := (others => False);
      Empty : Boolean := True;
      Result : Filter := (Set => (others => False));
   begin
      while Event = S_Expressions.Events.Add_Atom loop
         for Octet of Arguments.Current_Atom loop
            Set (Octet) := True;
            Empty := False;
         end loop;

         Arguments.Next (Event);
      end loop;

      if Empty then
         Result.Set := (others => True);
         return Result;
      end if;

      if Set (Character'Pos ('<')) or Set (Character'Pos ('>')) then
         Result.Set.Gt_Lt := True;

         if Set (Character'Pos ('<')) /= Set (Character'Pos ('>')) then
            Log (Severities.Warning,
              "Escaping both angle brackets despite only one being selected");
         end if;

         Set (Character'Pos ('<')) := False;
         Set (Character'Pos ('>')) := False;
      end if;

      if Set (Character'Pos ('&')) then
         Result.Set.Amp := True;
         Set (Character'Pos ('&')) := False;
      end if;

      if Set (Character'Pos (''')) then
         Result.Set.Apos := True;
         Set (Character'Pos (''')) := False;
      end if;

      if Set (Character'Pos ('"')) then
         Result.Set.Quot := True;
         Set (Character'Pos ('"')) := False;
      end if;

      for Octet in Set'Range loop
         if Set (Octet) then
            Log (Severities.Error, "Don't know how to escape "
              & Character'Image (Character'Val (Octet)));
         end if;
      end loop;

      return Result;
   end Create;

end Natools.Web.Escapes.Filters;
