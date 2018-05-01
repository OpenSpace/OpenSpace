import React from 'react'

const Dot = ({ id, active, dotClick }) => {
  const dotType = active ? 'dot active' : 'dot'
  return <div className={dotType} onClick={() => dotClick(id)} />
}

export default Dot;