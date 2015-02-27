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

------------------------------------------------------------------------------
-- Natools.Web.Fallback_Render is a render procedure that implements        --
-- generic commands, meant to be available from any context.                --
-- When the command is not recognized and Context is provided, the          --
-- following error message is logged:                                       --
-- Unknown render command "<command>" in <context>                          --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;
with Natools.Web.Sites;

procedure Natools.Web.Fallback_Render
  (Exchange : in out Natools.Web.Sites.Exchange;
   Name : in Natools.S_Expressions.Atom;
   Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class;
   Context : in String := "";
   Severity : in Severities.Code := Severities.Error);
