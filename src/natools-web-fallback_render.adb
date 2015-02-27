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

with Ada.Calendar;
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.Web.Fallback_Render;
with Natools.Web.Escapes;

procedure Natools.Web.Fallback_Render
  (Exchange : in out Natools.Web.Sites.Exchange;
   Name : in Natools.S_Expressions.Atom;
   Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class;
   Context : in String := "";
   Severity : in Severities.Code := Severities.Error)
is
   package Commands renames Natools.Static_Maps.Web.Fallback_Render;
   use type S_Expressions.Events.Event;
begin
   case Commands.To_Command (S_Expressions.To_String (Name)) is
      when Commands.Unknown =>
         if Context /= "" then
            Log (Severity, "Unknown render command """
              & S_Expressions.To_String (Name)
              & """ in "
              & Context);
         end if;

      when Commands.Current_Time =>
         S_Expressions.Templates.Dates.Render
           (Exchange, Arguments, Ada.Calendar.Clock);

      when Commands.Parameter =>
         if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            Escapes.Write
              (Exchange,
               Exchange.Parameter
                 (S_Expressions.To_String (Arguments.Current_Atom)),
               Escapes.HTML_Attribute);
         end if;

      when Commands.Set_MIME_Type =>
         if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            Exchange.Set_MIME_Type (Arguments.Current_Atom);
         end if;
   end case;
end Natools.Web.Fallback_Render;
