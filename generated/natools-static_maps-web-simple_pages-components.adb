with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Simple_Pages.Components is

   P : constant array (0 .. 1) of Natural :=
     (1, 8);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (2, 4);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (9, 9);

   G : constant array (0 .. 14) of Unsigned_8 :=
     (0, 0, 4, 3, 0, 0, 0, 0, 4, 1, 0, 0, 6, 2, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 15;
         F2 := (F2 + Natural (T2 (K)) * J) mod 15;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 7;
   end Hash;

end Natools.Static_Maps.Web.Simple_Pages.Components;
