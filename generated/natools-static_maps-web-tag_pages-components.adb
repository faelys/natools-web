with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tag_Pages.Components is

   P : constant array (0 .. 5) of Natural :=
     (1, 5, 9, 14, 18, 24);

   T1 : constant array (0 .. 5) of Unsigned_8 :=
     (41, 18, 12, 22, 20, 39);

   T2 : constant array (0 .. 5) of Unsigned_8 :=
     (21, 12, 19, 39, 35, 24);

   G : constant array (0 .. 46) of Unsigned_8 :=
     (0, 6, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 15, 0, 20, 0, 0, 17, 0, 0, 19,
      12, 0, 4, 2, 0, 0, 0, 9, 0, 3, 9, 0, 5, 0, 1, 0, 1, 13, 21, 6, 11, 0,
      12, 0, 3);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 47;
         F2 := (F2 + Natural (T2 (K)) * J) mod 47;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 23;
   end Hash;

end Natools.Static_Maps.Web.Tag_Pages.Components;
