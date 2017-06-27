------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Conditionals.Strings;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Templates.Dates;
with Natools.Static_Maps.Web.Fallback_Render;
with Natools.Web.ACL;
with Natools.Web.Comment_Cookies;
with Natools.Web.Escapes;
with Natools.Web.Filters.Stores;
with Natools.Web.Tags;
with Natools.Web.String_Tables;

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

   procedure Render_Ref (Ref : in S_Expressions.Atom_Refs.Immutable_Reference);
   procedure Report_Unknown_Command;

   procedure Render_Ref
     (Ref : in S_Expressions.Atom_Refs.Immutable_Reference) is
   begin
      if not Ref.Is_Empty then
         Escapes.Write
           (Exchange,
            S_Expressions.To_String (Ref.Query),
            Escapes.HTML_Attribute);
      elsif Re_Enter /= null then
         Re_Enter (Exchange, Arguments);
      end if;
   end Render_Ref;

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

      when Commands.Cookies =>
         String_Tables.Render
           (Exchange,
            String_Tables.Create (Exchange.Cookie_Table),
            Arguments);

      when Commands.Comment_Cookie_Filter =>
         Render_Ref (Comment_Cookies.Filter (Sites.Comment_Info (Exchange)));

      when Commands.Comment_Cookie_Link =>
         Render_Ref (Comment_Cookies.Link (Sites.Comment_Info (Exchange)));

      when Commands.Comment_Cookie_Mail =>
         Render_Ref (Comment_Cookies.Mail (Sites.Comment_Info (Exchange)));

      when Commands.Comment_Cookie_Name =>
         Render_Ref (Comment_Cookies.Name (Sites.Comment_Info (Exchange)));

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

      when Commands.If_Comment_Cookie_Filter_Is =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            declare
               use type S_Expressions.Atom;
               Ref : constant S_Expressions.Atom_Refs.Immutable_Reference
                 := Comment_Cookies.Filter (Sites.Comment_Info (Exchange));
            begin
               if not Ref.Is_Empty
                 and then Ref.Query = Arguments.Current_Atom
               then
                  Arguments.Next;
                  Re_Enter (Exchange, Arguments);
               end if;
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

      when Commands.If_Header_Else =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Open_List then
            declare
               Lock : S_Expressions.Lockable.Lock_State;
               Event : S_Expressions.Events.Event;
               Condition : Boolean;
            begin
               Arguments.Next (Event);
               if Event /= S_Expressions.Events.Add_Atom then
                  return;
               end if;

               Arguments.Lock (Lock);

               Evaluate_Condition :
               declare
                  Value : constant String := Exchange.Header
                    (S_Expressions.To_String (Arguments.Current_Atom));
               begin
                  Arguments.Next;
                  Condition := S_Expressions.Conditionals.Strings.Evaluate
                    (Value, Arguments);
                  Arguments.Unlock (Lock);
               exception
                  when others =>
                     Arguments.Unlock (Lock, False);
                     raise;
               end Evaluate_Condition;

               Arguments.Next (Event);

               case Event is
                  when S_Expressions.Events.Add_Atom =>
                     if Condition then
                        Exchange.Append (Arguments.Current_Atom);
                     end if;

                  when S_Expressions.Events.Open_List =>
                     if Condition then
                        Arguments.Lock (Lock);
                        begin
                           Arguments.Next;
                           Re_Enter (Exchange, Arguments);
                           Arguments.Unlock (Lock);
                        exception
                           when others =>
                              Arguments.Unlock (Lock, False);
                              raise;
                        end;
                     else
                        Arguments.Close_Current_List;
                     end if;

                  when S_Expressions.Events.Close_List
                    | S_Expressions.Events.End_Of_Input
                    | S_Expressions.Events.Error
                  =>
                     return;
               end case;

               if Condition then
                  return;
               end if;

               Arguments.Next (Event);

               case Event is
                  when S_Expressions.Events.Add_Atom =>
                     Exchange.Append (Arguments.Current_Atom);

                  when S_Expressions.Events.Open_List =>
                     Arguments.Lock (Lock);
                     begin
                        Arguments.Next;
                        Re_Enter (Exchange, Arguments);
                        Arguments.Unlock (Lock);
                     exception
                        when others =>
                           Arguments.Unlock (Lock, False);
                           raise;
                     end;

                  when S_Expressions.Events.Close_List
                    | S_Expressions.Events.End_Of_Input
                    | S_Expressions.Events.Error
                  =>
                     return;
               end case;
            end;
         end if;

      when Commands.If_Parameter_Is =>
         if Re_Enter = null then
            Report_Unknown_Command;
         elsif Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            declare
               Param_Value : constant String := Exchange.Parameter
                 (S_Expressions.To_String (Arguments.Current_Atom));
               Event : S_Expressions.Events.Event;
            begin
               Arguments.Next (Event);
               if Event = S_Expressions.Events.Add_Atom
                 and then Param_Value
                    = S_Expressions.To_String (Arguments.Current_Atom)
               then
                  Arguments.Next;
                  Re_Enter (Exchange, Arguments);
               end if;
            end;
         end if;

      when Commands.Load_Date =>
         S_Expressions.Templates.Dates.Render
           (Exchange, Arguments, Exchange.Site.Load_Date);

      when Commands.Optional_Tags =>
         Tags.Render
           (Exchange, Exchange.Site.Get_Tags, Arguments, Optional => True);

      when Commands.Parameter =>
         if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
            declare
               Parameter_Name : constant String
                 := S_Expressions.To_String (Arguments.Current_Atom);
            begin
               if Exchange.Has_Parameter (Parameter_Name) then
                  Escapes.Write
                    (Exchange,
                     Exchange.Parameter (Parameter_Name),
                     Escapes.HTML_Attribute);
               elsif Re_Enter /= null then
                  Arguments.Next;
                  Re_Enter (Exchange, Arguments);
               end if;
            end;
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

      when Commands.User =>
         if Re_Enter = null then
            Report_Unknown_Command;
         else
            declare
               Match : Boolean := False;
            begin
               ACL.Match (Sites.Identity (Exchange), Arguments, Match);

               if Match then
                  Arguments.Next;
                  Re_Enter (Exchange, Arguments);
               end if;
            end;
         end if;
   end case;
end Natools.Web.Fallback_Render;
