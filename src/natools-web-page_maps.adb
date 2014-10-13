------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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

package body Natools.Web.Page_Maps is

   procedure Get
     (Container : in Raw_Maps.Constant_Map;
      Path : in S_Expressions.Atom;
      Page : out Pages.Page_Ref;
      Found : out Boolean;
      Extra_Path_First : out S_Expressions.Offset)
   is
      Cursor : constant Raw_Maps.Cursor := Container.Floor (Path);
   begin
      if not Raw_Maps.Has_Element (Cursor) then
         Found := False;
         return;
      end if;

      declare
         use type S_Expressions.Atom;
         use type S_Expressions.Offset;

         Found_Path : constant S_Expressions.Atom := Raw_Maps.Key (Cursor);
      begin
         if Found_Path'Length > Path'Length
           or else Path (Path'First .. Path'First + Found_Path'Length - 1)
              /= Found_Path
         then
            Found := False;
            return;
         end if;

         Extra_Path_First := Path'First + Found_Path'Length;
      end;

      if Extra_Path_First in Path'Range
        and then Path (Extra_Path_First)
           not in Character'Pos ('/') | Character'Pos ('?')
      then
         Found := False;
         return;
      end if;

      Found := True;
      Page := Raw_Maps.Element (Cursor);
   end Get;

end Natools.Web.Page_Maps;
