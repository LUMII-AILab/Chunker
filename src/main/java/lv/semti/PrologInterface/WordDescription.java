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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import lv.semti.morphology.analyzer.MarkupConverter;
/**
 * Prolog term describing one node of syntax tree.
 */
public class WordDescription {
	private String description;
	private WordDescription dependencyHead = null;
	
	public WordDescription(String description) {
		this.description = description;
	}

	/**
	 * @return full description.
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * @return morphological markup.
	 */
	public String getTag() {
		return MarkupConverter.removeKamolsMarkupFormating(description);
	}
	
	/**
	 * @return wordform/name of x-word/punctuation mark.
	 */
	public String getForm() {
		String result = ""; 
		
		if (!description.contains(",")) return description;
		//FIXME - saks, bet workaround
		if (description.contains("\\',\\'")) return ",";
		
		int dziļums = 0;
		int komati = 0;
		for (char c : description.toCharArray()) {
			if (c=='[') dziļums++;
			if (c==']') dziļums--;
			if (dziļums == 0 && c==',') komati++;
			
			if (komati == 1) result = result + c; 
		}
		
		result = result.replaceAll("_[A-Z0-9]*"   ,   "_");             		
		result = result.replaceAll("(\\[|\\]|\\,| |'|\\\\)","");
		return result;
	}
	
	/**
	 * @return role.
	 */
	public String getRole() {
		String result = ""; 
		
		int dziļums = 0;
		int komati = 0;
		for (char c : description.toCharArray()) {
			if (c=='[') dziļums++;
			if (c==']') dziļums--;
			if ((dziļums == 1 || (dziļums == 2)) && c==',') komati++;
			
			if (dziļums == 2 && komati == 0) result = result + c; 
		}
		
		result = result.replaceAll("_[A-Z0-9]*"   ,   "_");             		
		result = result.replaceAll("(\\[|\\]|\\,| )","");
		result = result.replaceAll("_"," ");
		return result;	
	}

	/**
	 * Get link to the head element which this element is dependent of. 
	 */
	public WordDescription getDependencyHead() {
		return dependencyHead;
	}

	/**
	 * Set link to the head element which this element is dependent of. 
	 */
	public void setDependencyHead(WordDescription head) {
		this.dependencyHead = head;
	}
	
	/**
	 * Detect whether chunk's part described by this object is an x-word.
	 */
	public boolean isXWord(){
		return description.matches(".*,\\s*xdefs\\s*,.*");
	}
	
	/**
	 * Detect whether chunk's part described by this object is part of an x-word.
	 * Returns positive also for roots and other undependent nodes.
	 */
	public boolean isPartOfXWord()
	{
		return description.matches("\\(?\\[x, ?x.*");
	}
	
	/**
	 * @return	if tag is in form "a, b, c, [d, e, f]", returns the part in
	 * 			"[]", else null. 
	 */
	public String getAdittionalTag()
	{
		Pattern p = Pattern.compile("^\\[.*,.*,\\s*\\[.*\\[(.*)\\]\\]");
		Matcher m = p.matcher(description);
		if (m.find())
			return m.group(1);
		return null;
	}
}
