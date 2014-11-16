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

with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.Error_Pages;

package body Natools.Web.Error_Pages is

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context;
      Data : in S_Expressions.Atom);

   procedure Default_Page
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context);

   procedure Execute
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);



   -------------------------
   -- Error Page Renderer --
   -------------------------

   procedure Append
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchanges.Append (Exchange, Data);
   end Append;


   procedure Default_Page
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context)
   is
   begin
      Exchanges.Append (Exchange, S_Expressions.To_Atom
        ("<html><head><title>Error "));
      Exchanges.Append (Exchange, Context.Code);
      Exchanges.Append (Exchange, S_Expressions.To_Atom
        ("</title></head><body><h1>Error "));
      Exchanges.Append (Exchange, Context.Code);
      Exchanges.Append (Exchange, S_Expressions.To_Atom
        ("</h1><p>"));
      Exchanges.Append (Exchange, S_Expressions.To_Atom
        (Natools.Static_Maps.Web.Error_Pages.To_Message
           (S_Expressions.To_String (Context.Code))));
      Exchanges.Append (Exchange, S_Expressions.To_Atom
        ("</p></body></html>"));
   end Default_Page;


   procedure Execute
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Arguments);
      use Natools.Static_Maps.Web.Error_Pages;
   begin
      case To_Command (S_Expressions.To_String (Name)) is
         when Unknown_Command =>
            null;

         when Message =>
            Exchanges.Append
              (Exchange,
               S_Expressions.To_Atom (To_Message
                 (S_Expressions.To_String (Context.Code))));

         when Path =>
            Exchanges.Append
              (Exchange,
               S_Expressions.To_Atom (Exchanges.Path (Exchange)));

         when Status_Code =>
            Exchanges.Append (Exchange, Context.Code);
      end case;
   end Execute;


   procedure Render is new S_Expressions.Interpreter_Loop
     (Exchanges.Exchange, Error_Context, Execute, Append);



   -----------------------
   -- Private Interface --
   -----------------------

   procedure Main_Render
     (Exchange : in out Exchanges.Exchange;
      Context : in Error_Context)
   is
      use type S_Expressions.Atom;
      Template : S_Expressions.Caches.Cursor;
      Found : Boolean;
   begin
      Sites.Get_Template
        (Context.Site.all,
         S_Expressions.To_Atom ("error-page-") & Context.Code,
         Template,
         Found);

      if not Found then
         Sites.Get_Template
           (Context.Site.all,
            S_Expressions.To_Atom ("error-page"),
            Template,
            Found);

         if not Found then
            Default_Page (Exchange, Context);
            return;
         end if;
      end if;

      Render (Template, Exchange, Context);
   end Main_Render;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Check_Method
     (Exchange : in out Exchanges.Exchange;
      Site : aliased in Sites.Site;
      Allowed_Set : in Exchanges.Method_Set;
      Allowed : out Boolean) is
   begin
      Allowed := Exchanges.Is_In (Exchanges.Method (Exchange), Allowed_Set);

      if not Allowed then
         Method_Not_Allowed (Exchange, Site, Allowed_Set);
      end if;
   end Check_Method;


   procedure Check_Method
     (Exchange : in out Exchanges.Exchange;
      Site : aliased in Sites.Site;
      Allowed_Methods : in Exchanges.Method_Array;
      Allowed : out Boolean) is
   begin
      Check_Method
        (Exchange, Site, Exchanges.To_Set (Allowed_Methods), Allowed);
   end Check_Method;


   procedure Check_Method
     (Exchange : in out Exchanges.Exchange;
      Site : aliased in Sites.Site;
      Allowed_Method : in Exchanges.Known_Method;
      Allowed : out Boolean) is
   begin
      Check_Method (Exchange, Site, (1 => Allowed_Method), Allowed);
   end Check_Method;


   procedure Method_Not_Allowed
     (Exchange : in out Exchanges.Exchange;
      Site : aliased in Sites.Site;
      Allow : in Exchanges.Method_Set)
   is
      Context : constant Error_Context
        := (Site => Site'Access,
            Code => S_Expressions.To_Atom ("405"));
   begin
      Exchanges.Method_Not_Allowed (Exchange, Allow);
      Main_Render (Exchange, Context);
   end Method_Not_Allowed;


   procedure Not_Found
     (Exchange : in out Exchanges.Exchange;
      Site : aliased in Sites.Site)
   is
      Context : constant Error_Context
        := (Site => Site'Access,
            Code => S_Expressions.To_Atom ("404"));
   begin
      Exchanges.Not_Found (Exchange);
      Main_Render (Exchange, Context);
   end Not_Found;

end Natools.Web.Error_Pages;