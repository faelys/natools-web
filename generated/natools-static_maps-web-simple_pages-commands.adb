with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Simple_Pages.Commands is

   P : constant array (0 .. 3) of Natural :=
     (1, 4, 7, 8);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (10, 0, 13, 16);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (6, 10, 1, 21);

   G : constant array (0 .. 29) of Unsigned_8 :=
     (0, 0, 9, 10, 4, 2, 10, 0, 1, 3, 0, 7, 0, 0, 0, 0, 11, 6, 0, 8, 2, 0,
      0, 0, 0, 0, 0, 8, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 30;
         F2 := (F2 + Natural (T2 (K)) * J) mod 30;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 14;
   end Hash;

end Natools.Static_Maps.Web.Simple_Pages.Commands;
