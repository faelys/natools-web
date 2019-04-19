------------------------------------------------------------------------------
-- Copyright (c) 2014-2019, Natacha Porté                                   --
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

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Static_Maps.Web.Error_Pages;
with Natools.Web.Containers;
with Natools.Web.Fallback_Render;

package body Natools.Web.Error_Pages is

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context;
      Data : in S_Expressions.Atom);

   procedure Default_Page
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context);

   procedure Execute
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);


   procedure Render is new S_Expressions.Interpreter_Loop
     (Sites.Exchange, Error_Context, Execute, Append);



   -------------------------
   -- Error Page Renderer --
   -------------------------

   procedure Append
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context;
      Data : in S_Expressions.Atom)
   is
      pragma Unreferenced (Context);
   begin
      Exchange.Append (Data);
   end Append;


   procedure Default_Page
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context)
   is
   begin
      Exchange.Append (S_Expressions.To_Atom ("<html><head><title>Error "));
      Exchange.Append (Context.Code);
      Exchange.Append
        (S_Expressions.To_Atom ("</title></head><body><h1>Error "));
      Exchange.Append (Context.Code);
      Exchange.Append (S_Expressions.To_Atom ("</h1><p>"));
      Exchange.Append
        (S_Expressions.To_Atom
           (Natools.Static_Maps.Web.Error_Pages.To_Message
              (S_Expressions.To_String (Context.Code))));
      Exchange.Append (S_Expressions.To_Atom ("</p></body></html>"));
   end Default_Page;


   procedure Execute
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class);

      procedure Re_Enter
        (Exchange : in out Sites.Exchange;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is
      begin
         Render (Expression, Exchange, Context);
      end Re_Enter;

      use Natools.Static_Maps.Web.Error_Pages;
   begin
      case To_Command (S_Expressions.To_String (Name)) is
         when Unknown_Command =>
            Fallback_Render
              (Exchange, Name, Arguments,
               "error page", Re_Enter'Access);

         when Location =>
            if not Context.Location.Is_Empty then
               Exchange.Append (Context.Location.Query);
            end if;

         when Message =>
            Exchange.Append
              (S_Expressions.To_Atom (To_Message
                 (S_Expressions.To_String (Context.Code))));

         when Path =>
            Exchange.Append
              (S_Expressions.To_Atom (Exchanges.Path (Exchange)));

         when Status_Code =>
            Exchange.Append (Context.Code);
      end case;
   end Execute;



   -----------------------
   -- Private Interface --
   -----------------------

   procedure Main_Render
     (Exchange : in out Sites.Exchange;
      Context : in Error_Context)
   is
      use type S_Expressions.Atom;
      Expression : Containers.Optional_Expression;
   begin
      Expression := Exchange.Site.Get_Template
        (S_Expressions.To_Atom ("error-page-") & Context.Code);

      if Expression.Is_Empty then
         Expression := Exchange.Site.Get_Template
           (S_Expressions.To_Atom ("error-page"));
      end if;

      if Expression.Is_Empty then
         Default_Page (Exchange, Context);
      else
         Render (Expression.Value, Exchange, Context);
      end if;
   end Main_Render;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Set : in Exchanges.Method_Set;
      Allowed : out Boolean) is
   begin
      Allowed := Exchanges.Is_In (Exchanges.Method (Exchange), Allowed_Set);

      if not Allowed then
         Method_Not_Allowed (Exchange, Allowed_Set);
      end if;
   end Check_Method;


   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Methods : in Exchanges.Method_Array;
      Allowed : out Boolean) is
   begin
      Check_Method (Exchange, Exchanges.To_Set (Allowed_Methods), Allowed);
   end Check_Method;


   procedure Check_Method
     (Exchange : in out Sites.Exchange;
      Allowed_Method : in Exchanges.Known_Method;
      Allowed : out Boolean) is
   begin
      Check_Method (Exchange, (1 => Allowed_Method), Allowed);
   end Check_Method;


   procedure Method_Not_Allowed
     (Exchange : in out Sites.Exchange;
      Allow : in Exchanges.Method_Set)
   is
      Context : constant Error_Context
        := (Code => S_Expressions.To_Atom ("405"),
            Location => <>);
   begin
      Exchanges.Method_Not_Allowed (Exchange, Allow);
      Main_Render (Exchange, Context);
   end Method_Not_Allowed;


   procedure Not_Found (Exchange : in out Sites.Exchange) is
      Context : constant Error_Context
        := (Code => S_Expressions.To_Atom ("404"),
            Location => <>);
   begin
      Exchanges.Not_Found (Exchange);
      Main_Render (Exchange, Context);
   end Not_Found;


   procedure Permanent_Redirect
     (Exchange : in out Sites.Exchange;
      Location : in S_Expressions.Atom)
   is
      Context : constant Error_Context
        := (Code => S_Expressions.To_Atom ("301"),
            Location => S_Expressions.Atom_Ref_Constructors.Create (Location));
   begin
      Exchange.Permanent_Redirect (Context.Location);
      Main_Render (Exchange, Context);
   end Permanent_Redirect;


   procedure See_Other
     (Exchange : in out Sites.Exchange;
      Location : in S_Expressions.Atom)
   is
      Context : constant Error_Context
        := (Code => S_Expressions.To_Atom ("303"),
            Location => S_Expressions.Atom_Ref_Constructors.Create (Location));
   begin
      Exchange.See_Other (Context.Location);
      Main_Render (Exchange, Context);
   end See_Other;

end Natools.Web.Error_Pages;
