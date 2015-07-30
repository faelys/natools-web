with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 5) of Natural :=
     (3, 7, 8, 9, 10, 14);

   T1 : constant array (0 .. 5) of Unsigned_8 :=
     (3, 12, 46, 5, 51, 44);

   T2 : constant array (0 .. 5) of Unsigned_8 :=
     (33, 18, 26, 25, 41, 18);

   G : constant array (0 .. 52) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 9, 0, 0, 22, 9, 0, 0, 0, 0, 21, 0, 0, 4, 8, 12, 0, 0,
      0, 0, 0, 17, 25, 0, 0, 3, 0, 0, 12, 0, 0, 1, 3, 5, 4, 25, 0, 25, 11,
      15, 13, 7, 5, 0, 25, 0, 1, 0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 53;
         F2 := (F2 + Natural (T2 (K)) * J) mod 53;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 26;
   end Hash;

end Natools.Static_Maps.Web.List_Templates.Commands;
