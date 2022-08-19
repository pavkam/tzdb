<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="text" />
  <xsl:template match="mapZone[@territory='001']">
    GlobalCache.AddAlias('<xsl:value-of select="@other" />', '<xsl:value-of select="@type" />');
  </xsl:template>
</xsl:stylesheet>