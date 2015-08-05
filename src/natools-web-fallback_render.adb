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
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.Web.Fallback_Render;
with Natools.Web.Escapes;
with Natools.Web.Filters.Stores;
with Natools.Web.Tags;

procedure Natools.Web.Fallback_Render
  (Exchange : in out Natools.Web.Sites.Exchange;
   Name : in Natools.S_Expressions.Atom;
   Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class;
   Context : in String := "";
   Re_Enter : access procedure
     (Exchange : in out Natools.Web.Sites.Exchange;
      Expression : in out Natools.S_Expressions.Lockable.Descriptor'Class)
     := null;
   Elements : in Natools.Web.Containers.Expression_Maps.Constant_Map
     := Natools.Web.Containers.Expression_Maps.Empty_Constant_Map;
   Severity : in Severities.Code := Severities.Error)
is
   package Commands renames Natools.Static_Maps.Web.Fallback_Render;
   use type S_Expressions.Events.Event;

   procedure Report_Unknown_Command;

   procedure Report_Unknown_Command is
   begin
      if Context /= "" then
         Log (Severity, "Unknown render command """
           & S_Expressions.To_String (Name)
           & """ in "
           & Context);
      end if;
   end Report_Unknown_Command;
begin
   case Commands.To_Command (S_Expressions.To_String (Name)) is
      when Commands.Unknown =>
         Report_Unknown_Command;

      when Commands.Current_Time =>
         S_Expressions.Templates.Dates.Render
           (Exchange, Arguments, Ada.Calendar.Clock);

      when Commands.Element =>
         if Re_Enter = null then
            Report_Unknown_Command;
         else
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Elements,
                     Arguments,
                     Lookup_Template => False);
            begin
               Re_Enter (Exchange, Template);
            end;
         end if;

      when Commands.Element_Or_Template =>
         if Re_Enter = null then
            Report_Unknown_Command;
         else
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template (Elements, Arguments);
            begin
               Re_Enter (Exchange, Template);
            end;
         end if;

      when Commands.Filter =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            begin
               declare
                  Filter : Filters.Filter'Class
                    := Exchange.Site.Get_Filter (Arguments.Current_Atom);
               begin
                  Arguments.Next;
                  Exchange.Insert_Filter (Filter);
                  Re_Enter (Exchange, Arguments);
                  Exchange.Remove_Filter (Filter);
               end;
            exception
               when Filters.Stores.No_Filter => null;
            end;
         end if;

      when Commands.If_Has_Element =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Add_Atom
           and then Elements.Contains (Arguments.Current_Atom)
         then
            Arguments.Next;
            Re_Enter (Exchange, Arguments);
         end if;

      when Commands.If_Has_Not_Element =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Add_Atom
           and then not Elements.Contains (Arguments.Current_Atom)
         then
            Arguments.Next;
            Re_Enter (Exchange, Arguments);
         end if;

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

      when Commands.Tags =>
         Tags.Render (Exchange, Exchange.Site.Get_Tags, Arguments);

      when Commands.Template =>
         if Re_Enter = null then
            Report_Unknown_Command;
         else
            declare
               Template : S_Expressions.Caches.Cursor
                 := Exchange.Site.Get_Template
                    (Elements,
                     Arguments,
                     Lookup_Element => False);
            begin
               Re_Enter (Exchange, Template);
            end;
         end if;
   end case;
end Natools.Web.Fallback_Render;
