with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.List_Elements is

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 3);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 0);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 3);

   G : constant array (0 .. 12) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 4, 0, 3);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 13;
         F2 := (F2 + Natural (T2 (K)) * J) mod 13;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 5;
   end Hash;

end Natools.Static_Maps.Web.Comments.List_Elements;
