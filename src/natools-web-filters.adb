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

with Natools.S_Expressions.Atom_Buffers;

package body Natools.Web.Filters is

   ---------------------
   -- Stack Interface --
   ---------------------

   overriding procedure Apply
     (Object : in Stack;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array) is
   begin
      case Object.Backend.Length is
         when 0 =>
            Output.Write (Data);

         when 1 =>
            Apply (Object.Backend.First_Element, Output, Data);

         when others =>
            declare
               Buffer : S_Expressions.Atom_Buffers.Atom_Buffer;
            begin
               Buffer.Append (Data);

               for F of Object.Backend loop
                  declare
                     Previous : constant S_Expressions.Atom := Buffer.Data;
                  begin
                     Buffer.Soft_Reset;
                     Apply (F, Buffer, Previous);
                  end;
               end loop;

               Output.Write (Buffer.Data);
            end;
      end case;
   end Apply;


   not overriding procedure Insert
     (Container : in out Stack;
      Element : in Filter'Class;
      On : in Side := Top) is
   begin
      case On is
         when Top =>
            Container.Backend.Prepend (Element);
         when Bottom =>
            Container.Backend.Append (Element);
      end case;
   end Insert;


   not overriding procedure Remove
     (Container : in out Stack;
      Element : in Filter'Class;
      From : in Side := Top) is
   begin
      pragma Assert (not Container.Backend.Is_Empty);

      case From is
         when Top =>
            declare
               Removed : constant Filter'Class
                 := Container.Backend.First_Element;
            begin
               Container.Backend.Delete_First;
               if Removed /= Element then
                  raise Program_Error
                    with "Filters.Remove called with wrong Element";
               end if;
            end;

         when Bottom =>
            declare
               Removed : constant Filter'Class
                 := Container.Backend.Last_Element;
            begin
               Container.Backend.Delete_Last;
               if Removed /= Element then
                  raise Program_Error
                    with "Filters.Remove called with wrong Element";
               end if;
            end;
      end case;
   end Remove;

end Natools.Web.Filters;

