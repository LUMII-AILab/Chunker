/*******************************************************************************
 * Copyright 2008, 2009 Institute of Mathematics and Computer Science, University of Latvia; Author: Pēteris Paikens
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************/
package lv.semti.PrologInterface;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Hashtable;

import org.w3c.dom.Node;

import jpl.Query;
import jpl.Term;
import jpl.Variable;

/**
 * Prolog term describing syntax tree.
 */
public class ChunkerVariant {
	private String prologterms; //FIXME - jālikvidē un jāaizvieto ar vārdu aprakstu
	private ArrayList<WordDescription> vārdi = new ArrayList<WordDescription>();
	
	private WordDescription galvenais = null;
	
	private String html;
	
	public ChunkerVariant() {
		prologterms = "";
	}
	
	public ChunkerVariant(Term variants, String html) {
		prologterms = variants.toString();
		
		if (!variants.toString().equals("[]")) {
			Query query2 = new Query("parse_results_java2", new Term[] { variants, new Variable("OUT") });
			@SuppressWarnings("unchecked")
			Hashtable<String,Term> solution2 = query2.oneSolution();
		    Term paarveidots = solution2.get("OUT");
		    
			ChunkerVariant pirmaisČunks = apstaigātČunku(paarveidots.toString().substring(1,paarveidots.toString().length()-1));
			if (pirmaisČunks!=null) 
				this.pielikt(pirmaisČunks);
		}

		this.html = html;
	    pieliktHTMLHeaderi();
	}

	private void pieliktHTMLHeaderi() {
	    if (html.equals("")) return;
	    
	    String rezults = html.substring(1, html.length()-1);
    	
	    rezults = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">\n<head>\n" +
	    "<style>\n"+
	    "p.chunk-unknown { font-size: x-large; color: black; background-color: white; }\n"+
	    "table.chunk { border-spacing: 0px; padding: 0px 0px 0px 0px; text-align:center; background-color: white; }\n"+
	    "table.chunk tr { border-spacing: 0px; padding: 0px 0px 0px 0px; }\n"+

	    "td.chunk-word { font-size: x-large; padding: 5px 15px 5px 15px; border: 1px solid lightgray; }\n"+
	    "td.chunk-word-morphology { font-size: small; padding: 3px 10px 3px 10px; }\n"+
	    "td.chunk-word-concept { font-size: small; padding: 3px 10px 3px 10px; white-space: nowrap; }\n"+
	    "td.chunk-word-synrel { font-size: small; font-weight: bold; padding: 3px 10px 3px 10px; border-bottom: 2px solid blue; }\n"+
	    "td.chunk-word-semrole { font-size: small; font-weight: bold; padding: 3px 10px 3px 10px; border-top: 2px solid blue; }\n"+

	    "td.chunk-border-left { border-left: 2px solid blue; }\n"+
	    "td.chunk-border-right { border-right: 2px solid blue; }\n"+

	    "td.x-chunk-word { font-size: x-large; padding: 5px 15px 5px 15px; border: 1px solid lightgray; }\n"+
	    "td.x-chunk-word-morphology { font-size: small; padding: 3px 10px 3px 10px; }\n"+
	    "td.x-chunk-word-concept { font-size: small; padding: 3px 10px 3px 10px; white-space: nowrap; }\n"+
	    "td.x-chunk-word-synrel { font-size: small; font-weight: bold; padding: 3px 10px 3px 10px; border-bottom: 2px solid green; }\n"+
	    "td.x-chunk-word-semrole { font-size: small; font-weight: bold; padding: 3px 10px 3px 10px; border-top: 2px solid green; }\n"+

	    "td.x-chunk-border-left { border-left: 2px solid green; }\n"+
	    "td.x-chunk-border-right { border-right: 2px solid green; }\n"+

	    "span.word-info { font-size: x-small; }\n"+
	    "span.word-type { white-space: nowrap; }\n"+

	    "td.chunk-word-morphology span.morphology-code { font-size: small; font-family: courier; color: blue; white-space: nowrap; }\n"+
	    "td.x-chunk-word-morphology span.morphology-code { font-size: small; font-family: courier; color: green; white-space: nowrap; }\n"+

	    "p.sentence { font-size: x-large; font-weight: normal; padding: 10px 20px 10px 20px; border: 2px solid darkblue; background-color: white; }\n"+
	    "p.sentence span.sentence { font-size: x-large; font-weight: bold; color: darkblue; }\n"+
	    "p.result-title { font-size: large; font-weight: bold; }\n"+
	    "p.result-chunk { font-size: small; }\n"+
	    "p.result-chunk span.title { font-weight: bold; }\n"+
	    "p.parse-error { font-size: large; font-weight: bold; color: red; }\n"+

	    "div.sample-odd { padding: 10px 20px 10px 20px; border: 5px solid powderblue; background-color: powderblue; }\n"+
	    "div.sample-even { padding: 10px 20px 10px 20px; border: 5px solid khaki; background-color: khaki; }\n"+
	    "</style>\n"+   
	    		"<meta http-equiv=\"Content-Type\" content=\"text/html\" />\n<title>Čankeris</title>\n<link rel=\"stylesheet\" type=\"text/css\" href=\"style-box.css\" />\n</head>\n<body>" +
	    	rezults + "</body>";
	    
	    html = rezults;
	}

	public ChunkerVariant apstaigātČunku (String chunk) {

		// Quick and dirty. I don't know, why prolog puts ";" in braces.
		chunk = chunk.replace("(;)", ";");
		//chunk = chunk.replace(", ", ",");
		
		ChunkerVariant result = new ChunkerVariant();
				
		if (chunk.startsWith("[")) {			
			int depth = 0;
			StringBuilder sb = new StringBuilder();
			boolean escapeCharacterFlag = false;
			boolean inQuotes = false;
			
			for (char c : chunk.toCharArray()) {
				
				if (!escapeCharacterFlag && !inQuotes && (c==']' || c==')')) depth--;
				//if (depth == 1 && escapeCharacterFlag && (c=='\''))
				if (escapeCharacterFlag && (c=='\''))
				{
					inQuotes = !inQuotes;
				}
				if (depth >= 1 )
				{
					if ((depth == 1) && c == ',' && !inQuotes)
					{
						ChunkerVariant newChunkerVar = apstaigātČunku(sb.toString().trim());
						if (sb.toString().trim().startsWith("(")) {
							result.setGalvenais(newChunkerVar.galvenais);
						} else if (result.galvenais != null) {
							newChunkerVar.setGalvenais(result.galvenais);
						}
						result.pielikt(newChunkerVar);
						
						sb = new StringBuilder();
					} else sb.append(c); 					
				}
				if (c=='\\') escapeCharacterFlag = !escapeCharacterFlag;
				else escapeCharacterFlag = false;
				
				if (!escapeCharacterFlag && !inQuotes && (c=='[' || c=='(')) depth++;				
			}
			
			ChunkerVariant newChunkerVar = apstaigātČunku(sb.toString().trim());
			if (sb.toString().trim().startsWith("(")) {
				result.setGalvenais(newChunkerVar.galvenais);
			} else if (result.galvenais != null) {
				newChunkerVar.setGalvenais(result.galvenais);
			}
			result.pielikt(newChunkerVar);
			
		} else if (chunk.startsWith("(")) {
		    WordDescription newWord = new WordDescription(chunk.substring(1,chunk.length()-1).trim());
		    result.pielikt(newWord);
		    result.galvenais = newWord;				
		} else {
			WordDescription newWord = new WordDescription(chunk);
		    result.pielikt(newWord);
		    result.galvenais = newWord;
		}
		return result;
	}

	private void setGalvenais(WordDescription galvenais) {		
		this.galvenais = galvenais;
		for (WordDescription word : vārdi) 
			if (word.getDependencyHead() == null) {
				word.setDependencyHead(galvenais);
			}
	}

	public void pielikt(ChunkerVariant čunks) {
		if (čunks == null) return;
		for (WordDescription vārds : čunks.getVārdi()) {
			vārdi.add(vārds);
		}
	}
	
	public void pielikt(WordDescription vārdapraksts) {
		vārdi.add(vārdapraksts);
	}	

	public ArrayList<WordDescription> getVārdi() {
		return vārdi;
	}
	
	@Override
	public String toString() {
		/*
		String results = "variants";
		for (Vārdapraksts vārdapraksts : vārdi)
			results = results + "vārdapraksts: " + vārdapraksts.toString() + "\n";
		return results;
		*/
		return prologterms;
	}

	public String getPrologterms() {
		return prologterms;
	}

	public String getHTML() {
		return this.html;
	}

	public void uzXML(Writer straume) throws IOException {
		straume.write("<ČunkotājaVariants");
		straume.write(" prologterms=\"" + prologterms + "\">");	
		//TODO vi tik prologterms nejuaši nesatur XML formātam nedraudzīgus simbolus...?
		straume.write("</ČunkotājaVariants>\n");
	}
	
	
	public ChunkerVariant(Node node) {
		if (!node.getNodeName().equalsIgnoreCase("ČunkotājaVariants")) throw new Error("Node " + node.getNodeName() + " nav ČunkotājaVariants"); 
		prologterms = node.getAttributes().getNamedItem("prologterms").getTextContent();
 	}

}
