#This script should be run within the ArcGIS Pro Python window
import arcpy 

outfc = r"\\Mac\Home\Documents\Planning\DataManagement\DataManagement.gdb\TPA_Eligible_Transit_Stops_2017"

#Merge the following two feature classes together - map fields with the same content to single field
#Each of the FC below were added to the project from MTC Organization ArcGIS Online

railfc = r"TPA_Non_Bus_Eligible_Stops_2017"
busfc = r"High_Frequency_Bus_Stops"

#Create a new fieldmappings object and add the two input feature classes

fieldmappings = arcpy.FieldMappings()
fieldmappings.addTable(railfc)
fieldmappings.addTable(busfc)

#Get fieldmap for two fields in rail dataset which bus values will be merged into
#    - Create array of bus fields which are to be mapped (input fields)
#    - Create array of rail fields which bus fields will be mapped to (destination fields)
#    - Create dict using zip methold - ensure order of fields matches map - to order within array 

busfields = ["stop_id","lgcl_adjacent_hf_routes"]
railfields = ["agency_stop_id","Distance_Eligible"]
fieldmapdict = dict(zip(busfields,railfields))

#    - iterate over dictonary
#    - find field map for each rail field (destination field)
#    - add input field to fieldmap object (input field)
#    - replace field map (destination field map) within fieldmappings object with updated field
#    - remove bus field map from fieldmappings object (input field)

for busfield, railfield in fieldmapdict.items():
	fieldmap = fieldmappings.getFieldMap(fieldmappings.findFieldMapIndex(railfield))
	fieldmap.addInputField(busfc, busfield)
	fieldmappings.replaceFieldMap(fieldmappings.findFieldMapIndex(railfield), fieldmap)
	fieldmappings.removeFieldMap(fieldmappings.findFieldMapIndex(busfield)) 

#create temp value table and store fc paths as rows

vTab = arcpy.ValueTable()
vTab.addRow(railfc)
vTab.addRow(busfc)
#merge 

arcpy.Merge_management(vTab, outfc, fieldmappings)

#Calculate Transit System Field (system) as "bus" for all null values 

fieldName = r"system"
expression = r"transitSystemType(!system!)"
codeblock = r"""def transitSystemType(field):
    if (field is None):
        return "Bus"
    else:
        return field """

arcpy.management.CalculateField(outfc, fieldName, expression, "PYTHON_9.3", codeblock)

#Calculate Agency Name Field (agency_name) using Agency ID for all null values

fieldName = r"agency_name"
expression = r"agencyName(!agency_id!,!agency_name!)"
codeblock = r"""def agencyName(id, name):
    agencynamelookup = {"AC":"AC Transit", "EM":"Emmery Go Around", "SC":"VTA", "SF":"SFMTA", "SM":"SamTrans"}
    if (name is None):
        return agencynamelookup[id]
    else:
        return name """

arcpy.management.CalculateField(outfc, fieldName, expression, "PYTHON_9.3", codeblock)











