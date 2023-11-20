package pl.edu.pw.mini.zpoif.task4a.solutions;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Type;

import pl.edu.pw.mini.zpoif.task4a.Generator;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.Puchacz;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.Kabina;
import pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.przyrzady.Wariometr;

public class Solution {
	
	public static void main(String... args) {
		
		Puchacz puchacz = (Puchacz)Generator.utworzPuchacza();
		
		Wariometr w = stworzWariometr();
		wypiszKonstruktoryWiecejNizJedenParametr(puchacz);
		System.out.println(czySterowanyCiagiem(puchacz));
	}
	
	public static Wariometr stworzWariometr() {
		
		String nazwaKlasy = "pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.przyrzady.Wariometr";
		
		try {
			Class<?> klasa = Class.forName(nazwaKlasy);
			try {
				Object object = klasa.newInstance();
				Wariometr wariometr = (Wariometr)object;
				
				return wariometr;
				
			} catch (InstantiationException | IllegalAccessException e) {
				e.printStackTrace();
			}
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public static void pociagnijHamulce(Puchacz puchacz) {
		
		try {
			Class<?> clazz = Class.forName("pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec.HamulecAerodynamiczny");
			try {
				Method hamuj = clazz.getDeclaredMethod("hamuj", int.class);
				hamuj.setAccessible(true);
				Class<?> clazzPuchacz = Class.forName("pl.edu.pw.mini.zpoif.task4a.szybowiec.Puchacz");
				try {
					Object objekt = puchacz;
					Field field = clazzPuchacz.getDeclaredField("kabinaPierwsza");
					field.setAccessible(true);
					hamuj.invoke(field.get(objekt), new Integer(50));
				} catch(NoSuchFieldException | SecurityException e) {
					e.printStackTrace();
				}
			} catch(NoSuchMethodException | SecurityException e) {
				e.printStackTrace();
			}
		} catch(ClassNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	public static boolean czySterowanyCiagiem(Puchacz puchacz) {
		
		try {
			Class clazz1 = Class.forName("pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec.HamulecKolaZWada");
			Class clazz2 = Class.forName("pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm.MechanizmSterowanyCiegnem");
			
			return clazz2.isAssignableFrom(clazz1);
		} catch(ClassNotFoundException e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	public static void wypiszKonstruktoryWiecejNizJedenParametr(Puchacz puchacz) {
		
		Class<?> klasa = Puchacz.class;
		Class<?> superKlasa = klasa.getSuperclass();
		
		while (superKlasa != null) {
		
			Constructor<?>[] konstruktory = superKlasa.getConstructors();
			
			for (Constructor<?> konstruktor : konstruktory) {
	            if (konstruktor.getParameterCount() > 1) {
	                System.out.println(konstruktor);
	            }
	        }
			
			superKlasa = superKlasa.getSuperclass();
		}
	}
	
	public static void uruchomInstruktor(Puchacz puchacz) {
		
		try {
			Class<?> clazz = Class.forName("pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.inne.Instruktor");
			Method methods[] = clazz.getDeclaredMethods();
			
			for (Method method : methods) {
				Type returnType = method.getReturnType();
				if (!method.getReturnType().equals(void.class)) {
					
				}
			}
		} catch(ClassNotFoundException e) {
			e.printStackTrace();
		}
	}
}
