with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Tags.Commands is

   P : constant array (0 .. 1) of Natural :=
     (1, 11);

   T1 : constant array (0 .. 1) of Unsigned_8 :=
     (35, 18);

   T2 : constant array (0 .. 1) of Unsigned_8 :=
     (7, 4);

   G : constant array (0 .. 40) of Unsigned_8 :=
     (11, 11, 4, 11, 8, 18, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 6,
      0, 15, 0, 0, 0, 6, 0, 1, 0, 0, 0, 8, 16, 4, 2, 17, 0, 5, 13);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 41;
         F2 := (F2 + Natural (T2 (K)) * J) mod 41;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 20;
   end Hash;

end Natools.Static_Maps.Web.Tags.Commands;
